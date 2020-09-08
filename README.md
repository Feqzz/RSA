# RSA
RSA implementation in ADA. It currently encrypts/decrypts each character in the message separately, which is not ideal.

# Usage
`./rsa p q e Message`

# Example

Input:

`./rsa 47 53 7 Heisann`

Output:

```
p = 47
q = 53
e = 7
n = 2491
phi = 2392
d = 1367
message = Heisann
Encrypted message = <g`%G))
Decrypted message = Heisann
```
