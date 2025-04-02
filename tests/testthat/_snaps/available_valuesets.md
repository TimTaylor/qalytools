# errors are correct

    Code
      available_valuesets(mean)
    Condition
      Error in `.class_not_implemented()`:
      ! Not implemented for <function> objects.

---

    Code
      available_valuesets("bob")
    Condition
      Error in `available_valuesets()`:
      ! When `x` is a character object, it must be one of:
            - "eq5d5l", "EQ5D5L", "eq-5d-5l" or "EQ-5D-5L".
            - "eq5d3l", "EQ5D3L", "eq-5d-3l" or "EQ-5D-3L".
            - "eq5dy3l" , "EQ5DY3L" , "eq-5d-y-3l" or "EQ-5D-Y-3L".

