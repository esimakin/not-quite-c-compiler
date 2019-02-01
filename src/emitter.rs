use parser::*;

impl Visitor for ReturnStatement {
    fn visit(&self) -> String {
        let mut val = String::from("");
        val.push_str(&self.expr.visit());
        val.push_str("  ret\n");
        val.push_str("\n");
        val
    }
}

impl Visitor for Function {
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

impl Visitor for DeclareStatement {
    fn visit(&self) -> String {
        String::from("")
    }
}

impl Visitor for ExprStatement {
    fn visit(&self) -> String {
        String::from("")
    }
}

impl Visitor for IntExpression {
    fn visit(&self) -> String {
        let mut val = String::from("");
        val.push_str("  movl $");
        val.push_str(&self.val.to_string());
        val.push_str(", %eax\n");
        val
    }
}

impl Visitor for AssignExpression {
    fn visit(&self) -> String {
        String::from("")
    }
}

impl Visitor for VarExpression {
    fn visit(&self) -> String {
        String::from("")
    }
}

impl Visitor for UnaryOpExpression {
    fn visit(&self) -> String {
        let mut val = String::from("");
        val.push_str(&self.expr.visit());
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

use parser::BinOpType::*;

impl Visitor for BinOpExpression {
    fn visit(&self) -> String {
        let mut val = String::from("");
        match self.bin_op_type {
            Addition => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  pop %ecx\n");
                val.push_str("  addl %ecx, %eax\n");
            }
            Multiplication => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  pop %ecx\n");
                val.push_str("  imul %ecx, %eax\n");
            }
            Substraction => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  movl %eax, %ecx\n");
                val.push_str("  pop %eax\n");
                val.push_str("  subl %ecx, %eax\n");
            }
            Division => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  movl %eax, %ecx\n");
                val.push_str("  pop %eax\n");
                val.push_str("  movl $0, %edx\n");
                val.push_str("  idivl %ecx\n");
            }
            Or => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  pop %ecx\n");
                val.push_str("  orl %ecx, %eax\n");
                val.push_str("  movl $0, %eax\n");
                val.push_str("  setne %al\n");
            }
            And => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  pop %ecx\n");
                val.push_str("  cmpl $0, %ecx\n");
                val.push_str("  setne %cl\n");
                val.push_str("  cmpl $0, %eax\n");
                val.push_str("  movl $0, %eax\n");
                val.push_str("  setne %al\n");
                val.push_str("  %cl, %al\n");
            }
            Less | LessOrEq | Greater | GreaterOrEq | Equal | NotEqual => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  pop %ecx\n");
                val.push_str("  cmpl $0, %eax\n");
                val.push_str("  movl $0, %eax\n");
                match self.bin_op_type {
                    Less => val.push_str("  setl %al\n"),
                    LessOrEq => val.push_str("  setle %al\n"),
                    Greater => val.push_str("  setg %al\n"),
                    GreaterOrEq => val.push_str("  setge %al\n"),
                    Equal => val.push_str("  sete %al\n"),
                    NotEqual => val.push_str("  setne %al\n"),
                    _ => (),
                }
            }
        };
        val
    }
}

pub fn emit(program: Program) -> Result<String, &'static str> {
    let mut asm = String::from("");
    asm.push_str(&program.func.visit());
    return Ok(asm);
}
