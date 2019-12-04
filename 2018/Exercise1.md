
# DRAFT will continue later

### Advent of Code Part 2

The problem: we have an input list `nums : [Int]`, of frequency changes occurring cyclically. 
Assuming that the initial frequency is zero, the goal is to find the accumulated frequency that first appears twice.

First, let us start by writing some auxiliary definitions. 

``` Haskell
numsCy :: [a]
numsCy = cycle nums

accums :: Num a => [a]
accums = tail $ scanl (+) 0 numsCy -- tail: it adds 0 at beginning
```

We can make a bad  implementation of our problem as follows: 

```Haskell

-- Get: for each element

findEarliestRepeat = 
  minimum `on` snd3 . catMaybes . map findOneTail . tails . zip [0..] 

snd3 (_,x,_) = x

findOneTail :: (Num a, Eq a) => [(Int, a)] -> Maybe (Int, Int, a)
findOneTail ((i,a):bjs) == do 
    (j,_) <- findIndex ((== a) . fst) bjs
    (i,a,j)

```

This definition, of course, would never terminate since the `cycle` list is infinite and `findIndex` would run through the whole list. 
On the other hand, if we have `len = length nums`, then we could restrict `accums` as follows: 

``` Haskell
accums = tail . scanl (+) 0 . take (len * len) . cycle $ nums
```

The alternative strategy to solve this problem is to reduce the lists we are handling above. To do that, we only need to use that `sum (xs ++ ys) == sum xs + sum ys`. 



What this problem is asking as is to find the value `m` with the such that: 1) `m` is repeated in the list of accumualted frequencies. To write it equationally, there exists an `i,j` such that `m = accum i = accum j`; and 2) `m` is the value with a smallest such `i`.

Now, playing with the definitions, if we rewrite `i < j` as `j = i+k`, what we have is that `accum i = accum (i+k)`, which by expanding the definition becomes `(sum . take i . cycle $ xs) == (sum. take (i+k) . cycle $ xs)`, 
and since we know the following equations: 

- For any list `xs :: [a]`, for any number `i >= 0`, and for any number `j`  between `0` and `i`, it holds that `take i xs == take j xs ++ take (i-j) (drop j xs)`
- For any two lists `xs, ys` of a numeric type, it holds that `sum (xs ++ ys) == sum xs + sum ys`.

Using these two equations, we can rewrite the condition to `(sum . take i . cycle $ xs) == (sum. take i . cycle $ xs) + (sum . take k . drop i . cycle $ xs)`, which is equivalent to say that `(sum . take k . drop i . cycle $ xs) == 0`. 
So the problem is actually finding the first segment (cycled) with a zero sum. 

Now, if our target list is defined as `cycle xs`, from the input list, we can assume that it cannot encompass more than one repetition of the list. 

a. non-wrapping case: find two numbers `0 <= i < j <= length xs` such that `sum (drop i . take j) xs == 0`, or 
b. the wrapping case: find two numbers `0 <= i < j <= length xs` such that `(sum . take i $ xs) + (sum . drop j xs) == 0`

