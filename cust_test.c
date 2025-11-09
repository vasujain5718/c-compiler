int main(void) {
    // Outer declaration
    int x = 1;

    // Compound block + shadowing + if + null statement
    {
        int x = 2;          // shadows outer x
        if (x > 1) {        // true
            x = 3;
        } else ;            // test empty statement
        int x2 = 4;         // regular decl inside block
        ;                   // another explicit null statement
        // (Nothing from this block should affect the outer x)
    }

    // Ternary expression that must evaluate only one arm
    int flag = 0;
    int y = (x == 1) ? (flag = 10) : (flag = 20);  // expects y=10, flag=10

    // Short-circuit tests: RHS must NOT run
    int z = 0;
    int t_and = (0 && (z = 99));   // expects t_and=0, z unchanged
    int t_or  = (1 || (z = 88));   // expects t_or=1,  z unchanged

    // Assignment expression returns the assigned value
    int a = 0;
    int b = (a = 7);               // expects a=7, b=7

    // Unary ops and comparisons
    int not1 = !0;                 // 1
    int neg  = -5;                 // -5
    int cmp  = (3 * 4 == 12) + (5 < 2);  // 1 + 0 = 1

    // Final value checks block scoping and all above behaviors
    // result = 1 + 10 + 10 + 0 + 0 + 1 + 7 + 1 - 5 + 1 = 26
    int result = x + y + flag + z + t_and + t_or + b + not1 + neg + cmp;

    return result; // expect 26
}
