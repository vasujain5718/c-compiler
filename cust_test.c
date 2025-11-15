// test_relational.c
int main(void) {
    double x = 10.0;
    double y = 20.0;
    
    // Tests the "Unary Negate" fix
    double z = -30.0; 
    double w = 10.0;

    // Tests (10.0 < 20.0) -> setb
    if (x < y) { 
        // Tests (-30.0 < 10.0) -> setb
        if (z < x) { 
            // Tests (20.0 > -30.0) -> seta
            if (y > z) { 
                // Tests (10.0 != 20.0) -> setne
                if (x != y) { 
                    // Tests (10.0 == 10.0) -> sete
                    if (x == w) { 
                        // Tests (10.0 >= 10.0) -> setae
                        if (x >= w) {
                            // Tests (10.0 <= 10.0) -> setbe
                            if (x <= w) {
                                // Success!
                                return 100;
                            }
                            return 6; // Fail <=
                        }
                        return 5; // Fail >=
                    }
                    return 4; // Fail ==
                }
                return 3; // Fail !=
            }
            return 2; // Fail >
        }
        return 1; // Fail z < x
    }
    return 0; // Fail x < y
}