# RSA
RSA implementation in ADA. It does not support large numbers.

# Usage
```
rsa <p> <q> <e> <message>
rsa encrypt <n> <e> <message>
rsa decrypt <d> <n> <message>
```

# Example

Input:

`./rsa 47 53 7 100`

Output:

```
p = 47
q = 53
e = 7
n = 2491
phi = 2392
d = 1367
message = 100
Encrypted message = 2448
Decrypted message = 100
```
