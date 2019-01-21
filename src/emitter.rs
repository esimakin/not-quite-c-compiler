use parser::*;

impl Statement for ReturnStatement {
    fn visit(&self) -> String {
        let mut val = String::from("");
        val.push_str(&self.expression.visit());
        val.push_str("  ret\n");
        val.push_str("\n");
        val
    }
    fn to_string(&self) -> String {
        let mut val = String::from("ReturnStatement[");
        val.push_str(&self.expression.to_string());
        val.push_str("]");
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
    fn to_string(&self) -> String {
        let mut val = String::from("Function[");
        for stmt in self.statements.iter() {
            val.push_str(&stmt.to_string());
            val.push_str(" ");
        }
        val.push_str("]");
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
    fn to_string(&self) -> String {
        let mut val = String::from("IntExpressioin[");
        val.push_str(&self.val.to_string());
        val.push_str("]");
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
    fn to_string(&self) -> String {
        let mut val = String::from("UnaryOp[");
        val.push_str(self.unary_op_type.to_string());
        val.push_str("]");
        val
    }
}

impl Expression for BinaryOp {
    fn visit(&self) -> String {
        let mut val = String::from("");
        match self.bin_op_type {
            BinOpType::Addition => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  pop %ecx\n");
                val.push_str("  addl %ecx, %eax\n");
            }
            BinOpType::Multiplication => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  pop %ecx\n");
                val.push_str("  imul %ecx, %eax\n");
            }
            BinOpType::Substraction => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  movl %eax, %ecx\n");
                val.push_str("  pop %eax\n");
                val.push_str("  subl %ecx, %eax\n");
            }
            BinOpType::Division => {
                val.push_str(&self.left.visit());
                val.push_str("  push %eax\n");
                val.push_str(&self.right.visit());
                val.push_str("  movl %eax, %ecx\n");
                val.push_str("  pop %eax\n");
                val.push_str("  movl $0, %edx\n");
                val.push_str("  idivl %ecx\n");
            }
            BinOpType::And => {}
            BinOpType::Or => {}
            BinOpType::Less => {}
            BinOpType::LessOrEq => {}
            BinOpType::Greater => {}
            BinOpType::GreaterOrEq => {}
            BinOpType::Equal => {}
            BinOpType::NotEqual => {}
        };
        val
    }
    fn to_string(&self) -> String {
        let mut val = String::from("BinaryOp[");
        val.push_str(&self.left.to_string());
        val.push_str(self.bin_op_type.to_string());
        val.push_str(&self.right.to_string());
        val.push_str("]");
        val
    }
}

impl UnaryOpType {
    fn to_string(&self) -> &'static str {
        match self {
            UnaryOpType::Complement => "~",
            UnaryOpType::Negation => "-",
            UnaryOpType::LogicalNegation => "!",
        }
    }
}

impl BinOpType {
    fn to_string(&self) -> &'static str {
        match self {
            BinOpType::Addition => "+",
            BinOpType::Multiplication => "*",
            BinOpType::Substraction => "-",
            BinOpType::Division => "/",
            BinOpType::And => "&&",
            BinOpType::Or => "||",
            BinOpType::Less => "<",
            BinOpType::LessOrEq => "<=",
            BinOpType::Greater => ">",
            BinOpType::GreaterOrEq => ">=",
            BinOpType::Equal => "==",
            BinOpType::NotEqual => "!=",
        }
    }
}

impl Program {
    #[allow(dead_code)]
    pub fn to_string(&self) -> String {
        let mut val = String::from("Program[");
        for stmt in self.statements.iter() {
            val.push_str(&stmt.to_string());
        }
        val.push_str("]");
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
