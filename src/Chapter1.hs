{-# LANGUAGE NoImplicitPrelude #-}

module Chapter1 where

{-
All you need is Lambda

Combinators
Determine if each of the following are combinators or not.

1. \x.xxx
-> yes

2. \xy.zx
-> no

3. \xyz.xy(zx)
-> yes

4. \xyz.xy(zxy)
-> yes

5. \xy.xy(zxy)
-> no

Normal form or diverge?
Determine if each of the following can be reduced to a normal form of if they
diverge.

1. \x.xxx
-> normal form

2. (\z.zz)(\y.yy)
-> diverge

3. (\x.xxx)z
-> not a normal form but can be reduced further

Beta reduce
Evaluate (that is, beta reduce) each of the following expressions to normal
form. We strongly recommend writing out the steps on paper with a pencil or
pen.

1. (\abc.cba)zz(\wv.w)
(\wv.w)zz
z

2. (\x.\y.xyy)(\a.a)b
(\a.a)bb
bb

3. (\y.y)(\x.xx)(\z.zq)
(\x.xx)(\z.zq)
(\z.zq)(\z.zq)
(\z.zq)q
qq

4. (\z.z)(\z.zz)(\z.zy)
Hint: alpha equivalence.
(\z.zz)(\z.zy)
(\z.zy)(\z.zy)
(\z.zy)y
yy

5. (\x.\y.xyy)(\y.y)y
(\y.y)yy
yy

6. (\a.aa)(\b.ba)c
(\b.ba)(\b.ba)c
(\b.ba)ac
aac

7. (\xyz.xz(yz))(\x.z)(\x.a)
\z.(\x.z)z((\x.a)z)
\z.(\x.z)za
\z.za
Note that the z from the head and z from the body are different. The z from
the body is a free variable. Please see the expression (\x.z).

-}
