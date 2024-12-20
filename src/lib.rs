use swc_core::{
    common::{BytePos, SyntaxContext, Spanned, DUMMY_SP}, ecma::{
        ast::{
            ArrayLit, AssignExpr, AssignTarget, ClassDecl, ClassMember, Constructor, Expr, ExprOrSpread, ExprStmt, Ident, Lit, MemberExpr, MemberProp, ModuleItem, ParamOrTsParamProp, Pat, Program, Stmt, Str
        },
        transforms::testing::test_inline,
        visit::{visit_mut_pass, noop_visit_mut_type, VisitMut, VisitMutWith},
    }, plugin::{plugin_transform, proxies::TransformPluginProgramMetadata}
};

fn get_param_names(c: &Constructor) -> Vec<String> {
    let mut vec: Vec<String> = Vec::new();

    for p in c.params.iter() {
        if let ParamOrTsParamProp::Param(param) = p {
            match &param.pat {
                Pat::Ident(id) => vec.push(id.sym.to_string()),
                _ => {}
            }
        }
    }
    return vec;
}

fn array_string_member(str: &std::string::String) -> ExprOrSpread {
    let s = Str::from(str.clone());
    ExprOrSpread::from(Expr::Lit(Lit::Str(s)))
}

fn inject_ctor_param_names(p: &CtorParams) -> swc_core::ecma::ast::ExprStmt {
    let member_expr = MemberExpr {
        obj: Box::new(Expr::Ident(Ident::new(p.clone().ctor.into(), DUMMY_SP, SyntaxContext::empty()))),
        prop: MemberProp::Ident(Ident::new("$inject".into(), DUMMY_SP, SyntaxContext::empty()).into()),
        span: DUMMY_SP,
    };

    let assign_target = AssignTarget::try_from(member_expr).expect("Expected assign target");

    let array_members: Vec<Option<ExprOrSpread>> = p
        .params
        .iter()
        .map(|name| Some(array_string_member(name)))
        .collect();

    let array_expr = Expr::Array(ArrayLit {
        span: DUMMY_SP,
        elems: array_members,
    });

    let assign = AssignExpr {
        span: DUMMY_SP,
        op: swc_core::ecma::ast::AssignOp::Assign,
        left: assign_target,
        right: Box::new(array_expr),
    };

    ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(Expr::Assign(assign)),
    }
}

#[derive(Clone, Debug)]
struct CtorParams {
    ctor: String,
    params: Vec<String>,
    end_pos: BytePos
}

#[derive(Default)]
pub struct TransformVisitor {
    injects: Vec<Vec<CtorParams>>, //Vec<Vec<i64>>
}

impl VisitMut for TransformVisitor {
    noop_visit_mut_type!();

    fn visit_mut_class_decl(&mut self, n: &mut ClassDecl) {
        let class_name = n.ident.sym.to_string();

        let mut param_names: Option<Vec<String>> = Option::None;

        for member in n.class.body.iter() {
            if let ClassMember::Constructor(c) = member {
                param_names = Option::Some(get_param_names(c));
                break;
            }
        }

        if let Some(ref params) = param_names {
            let v = self.injects.last_mut().unwrap();
            let my_params = params.clone();
            v.push(CtorParams {
                ctor: class_name,
                params: my_params,
                end_pos: n.span_hi()
            });
        }
    }

    fn visit_mut_module_items(&mut self, n: &mut Vec<ModuleItem>) {
        self.injects.push(Vec::new());

        n.visit_mut_children_with(self);

        let injects = self.injects.pop().unwrap();
        for p in injects {
            let x = p;
            let expr_stmt = inject_ctor_param_names(&x);

            let end_pos = x.end_pos;
            let index_after = n.iter().position(|x: &ModuleItem| x.span_lo() > end_pos);

            let element = ModuleItem::Stmt(Stmt::Expr(expr_stmt));
            if let Some(index) = index_after {
                n.insert(index, element);
            }
            else
            {
                n.push(element);
            }
        }
    }

    fn visit_mut_stmts(&mut self, n: &mut Vec<Stmt>) {
        self.injects.push(Vec::new());

        n.visit_mut_children_with(self);

        let injects = self.injects.pop().unwrap();
        for p in injects {
            let x = p;
            let expr_stmt = inject_ctor_param_names(&x);

            let end_pos = x.end_pos;
            let index_after = n.iter().position(|x: &Stmt| x.span_lo() > end_pos);

            let element = Stmt::Expr(expr_stmt);
            if let Some(index) = index_after {
                n.insert(index, element);
            }
            else {
                n.push(element);
            }
        }        
    }
}

#[plugin_transform]
pub fn process_transform(mut program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    program.visit_mut_with(&mut visit_mut_pass(TransformVisitor {
        ..Default::default()
    }));

    program
}

test_inline!(
    Default::default(),
    |_| visit_mut_pass(TransformVisitor {
        ..Default::default()
    }),
    inject1,
    // Input codes
    r#"class HelloWorld { 
        constructor(a, b) { } 
    }
    angular.component("helloWorld", HelloWorld);"#,
    // Output codes after transformed with plugin
    r#"class HelloWorld { 
        constructor(a, b) { } 
    }
    HelloWorld.$inject = ["a", "b"];
    angular.component("helloWorld", HelloWorld);"#
);

test_inline!(
    Default::default(),
    |_| visit_mut_pass(TransformVisitor {
        ..Default::default()
    }),
    inject2,
    // Input codes
    r#"(function(){class HelloWorld { 
        constructor(a, b, janus) { } 
    }
    angular.component("helloWorld", HelloWorld);
    })();"#,
    // Output codes after transformed with plugin
    r#"(function(){class HelloWorld { 
        constructor(a, b, janus) { } 
    }
    HelloWorld.$inject = ["a", "b", "janus"];
    angular.component("helloWorld", HelloWorld);
    })();
    "#
);
