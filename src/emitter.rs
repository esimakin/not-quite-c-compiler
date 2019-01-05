use parser::*;

impl Node for ReturnStatement {
    fn visit(&self) -> String {
        let mut val = String::from("");
        val.push_str("  movl $");
        val.push_str(&self.expression.visit());
        val.push_str(", %eax\n");
        val.push_str("  ret\n");
        val.push_str("\n");
        val
    }
}

impl Node for MainFunction {
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

impl Node for IntExpression {
    fn visit(&self) -> String {
        String::from(self.val.to_string())
    }
}

pub fn emit(program: Program) -> Result<String, &'static str> {
    let mut asm = String::from("");
    asm.push_str(&program.main.visit());
    return Ok(asm);
}
