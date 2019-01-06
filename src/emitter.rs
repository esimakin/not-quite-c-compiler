use parser::*;

impl Statement for ReturnStatement {
    fn visit(&self) -> String {
        let mut val = String::from("");
        val.push_str(&self.expression.visit());
        val.push_str("  ret\n");
        val.push_str("\n");
        val
    }
}

impl Statement for Function {
    fn visit(&self) -> String {
        let mut val = String::from("  .text\n");
        val.push_str("  .globl  main\n");
        val.push_str("  .type main, @function\n");
        val.push_str("main:\n");
        for stmt in self.statements.iter() {
            val.push_str(&stmt.visit());
        }
        val
    }
}

impl Expression for IntExpression {
    fn visit(&self) -> String {
        let mut val = String::from("");
        val.push_str("  movl $");
        val.push_str(&self.val.to_string());
        val.push_str(", %eax\n");
        val
    }
}

impl Expression for UnaryOp {
    fn visit(&self) -> String {
        let mut val = String::from("");
        val.push_str(&self.expression.visit());
        match self.unary_op_type {
            UnaryOpType::Negation => {
                val.push_str("  neg %eax\n");
            }
            UnaryOpType::Complement => {
                val.push_str("  not %eax\n");
            }
            UnaryOpType::LogicalNegation => {
                val.push_str("  cmpl $0, %eax\n");
                val.push_str("  movl $0, %eax\n");
                val.push_str("  sete %al\n");
            }
        };
        val
    }
}

pub fn emit(program: Program) -> Result<String, &'static str> {
    let mut asm = String::from("");
    for stmt in &program.statements {
        asm.push_str(&stmt.visit());
    }
    return Ok(asm);
}
