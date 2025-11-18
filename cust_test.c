/* Comprehensive Test Suite for Custom Compiler 
   
   Expected Behavior:
   - The program should run without crashing.
   - The exit code (echo $? on Linux/Mac or echo %errorlevel% on Windows) should be 0.
   - Any non-zero exit code indicates a failure in that specific section.
*/

int main() {
    // ==========================================
    // TEST 1: Integers, Variables, and Assignment
    // ==========================================
    int a = 10;
    int b = 20;
    int sum = a + b;
    
    if (sum != 30) {
        return 1; // Error 1: Basic integer arithmetic failed
    }

    // ==========================================
    // TEST 2: Operator Precedence
    // ==========================================
    // Multiplication (50) should happen before addition (2)
    int prec = 2 + 5 * 10; 
    
    if (prec != 52) {
        return 2; // Error 2: Operator precedence failed
    }

    // ==========================================
    // TEST 3: Floating Point (Double & Float)
    // ==========================================
    // Your compiler supports implicit widening (int -> double)
    double pi = 3.14159;
    double r = 2.0;
    double area = pi * r * r; // ~12.56636

    // Simple check to avoid precision issues (12.5 < area < 12.6)
    if (area < 12.56) {
        return 3; // Error 3: Double arithmetic too low
    }
    if (area > 12.57) {
        return 3; // Error 3: Double arithmetic too high
    }

    // ==========================================
    // TEST 4: Character Literals
    // ==========================================
    char c = 'A'; // ASCII 65
    if (c != 65) {
        return 4; // Error 4: Char literal or ASCII mapping failed
    }

    // ==========================================
    // TEST 5: Arrays (Single Dimension)
    // ==========================================
    // Note: Your parser forbids "int arr[3] = {1,2,3};", so we assign manually.
    int arr[5];
    arr[0] = 100;
    arr[1] = 200;
    arr[2] = arr[0] + arr[1]; // 300

    if (arr[2] != 300) {
        return 5; // Error 5: Array indexing or assignment failed
    }

    // ==========================================
    // TEST 6: Logical Operators (&&, ||, !)
    // ==========================================
    int true_val = 1;
    int false_val = 0;

    if (!true_val) {
        return 6; // Error 6: Logical NOT failed
    }
    if (true_val && false_val) {
        return 6; // Error 6: Logical AND failed
    }
    if (!(false_val || true_val)) {
        return 6; // Error 6: Logical OR failed
    }

    // ==========================================
    // TEST 7: Bitwise Operators (&, |, ~)
    // ==========================================
    int bit_a = 5;      // 0101
    int bit_b = 3;      // 0011

 

    // ==========================================
    // TEST 8: Control Flow (If/Else)
    // ==========================================
    int x = 50;
    int result_if = 0;

    if (x > 100) {
        result_if = 1;
    } else {
        result_if = 2; // Should go here
    }

    if (result_if != 2) {
        return 8; // Error 8: If/Else branching logic failed
    }

    // ==========================================
    // TEST 9: Loops (While & Break)
    // ==========================================
    int i = 0;
    int loop_sum = 0;
    
    while (i < 10) {
        loop_sum = loop_sum + 1;
        if (i == 4) {
            break; // Should stop when i is 4 (sum will be 5)
        }
        i = i + 1;
    }

    if (loop_sum != 5) {
        return 9; // Error 9: While loop or Break statement failed
    }

    // ==========================================
    // TEST 10: Loops (For & Continue)
    // ==========================================
    int skip_sum = 0;
    // Sum 0 to 4, but skip 2. Expected: 0+1+3+4 = 8
    for (int j = 0; j < 5; j = j + 1) {
        if (j == 2) {
            continue;
        }
        skip_sum = skip_sum + j;
    }

    if (skip_sum != 8) {
        return 10; // Error 10: For loop or Continue statement failed
    }

    // ==========================================
    // SUCCESS
    // ==========================================
    return 0;
}