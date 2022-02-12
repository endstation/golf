mapping(string:int) HEX_DIGITS = ([
    "0":0, "1":1, "2":2, "3":3, "4":4, "5":5, "6":6, "7":7, "8":8, "9":9,
    "a":10, "b":11, "c":12, "d":13, "e":14, "f":15 ]);

int hex_str_to_int(string hs)
{
    string hs2 = String.trim_all_whites( lower_case(hs) );
    int exponent = 0;
    int sum = 0;

    for (int i = sizeof(hs2)-1; i >= 0; --i)
    {
        string digit = hs2[i..i];
        int decimal_value = HEX_DIGITS[digit] * pow(16, exponent);
        sum += decimal_value;
        ++exponent;
    } // for

    return sum;

} // hex_str_to_int

