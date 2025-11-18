/* Feature Test: Int, Char, Float, Arrays, Loops
   
   Constraint: 
   1. No &&, ||, &, | operators used.
   2. ARRAY INDICES MUST BE CONSTANT (e.g., arr[0], not arr[i])
      because codegen does not support dynamic indexing yet.
   
   Return codes:
   0 = Success
   1 = Float/Int/Char arithmetic mismatch
   2 = Array Read/Write failure
   3 = Loop logic failure
*/

int main() {
    // ==========================================
    // TEST 1: Mixed Type Arithmetic (int, char, double)
    // ==========================================
    int i_val = 10;
    char c_val = 'A'; // ASCII 65
    double f_val = 2.5;
    
    // Promotion: char(65) -> int(65) -> double(65.0)
    // Calculation: 10 + 65 + 2.5 = 77.5
    double result = i_val + c_val + f_val;

    if (result < 77.4) {
        return 1; 
    }
    if (result > 77.6) {
        return 1;
    }

    // ==========================================
    // TEST 2: Arrays (Static Access Only)
    // ==========================================
    // Since we can't use arr[i] yet, we test arrays manually.
    double arr[3];
    
    // Test Write
    arr[0] = 10.5;
    arr[1] = 20.5;
    arr[2] = 30.0;

    // Test Read & Sum
    double arr_sum = arr[0] + arr[1] + arr[2]; // Should be 61.0

    if (arr_sum < 60.9) {
        return 2; 
    }
    if (arr_sum > 61.1) {
        return 2;
    }

    // ==========================================
    // TEST 3: Loops (Logic Only)
    // ==========================================
    // We test the loop structure, but we use a separate counter
    // instead of using it to index an array.
    
    int loop_counter = 0;
    int idx = 0;
    
    // Run loop 5 times
    while (idx < 5) {
        loop_counter = loop_counter + 10;
        idx = idx + 1;
    }

    // Expected: 5 * 10 = 50
    if (loop_counter != 50) {
        return 3;
    }

    // ==========================================
    // SUCCESS
    // ==========================================
    return 0;
}