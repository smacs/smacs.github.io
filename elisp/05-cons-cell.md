---
layout: page
title: 基本数据类型之三 ── cons cell 和列表
---

如果从概念上来说，cons cell 其实非常简单的，就是两个有顺序的元素。第一个叫 CAR，第二个就 CDR。CAR 和 CDR 名字来自于 Lisp。它最初在IBM 704机器上的实现。在这种机器有一种取址模式，使人可以访问一个存储地址中的“地址（address）”部分和“减量（decrement）”部分。CAR 指令用于取出地址部分，表示(Contents of Address part of Register)，CDR 指令用于取出地址的减量部分(Contents of the Decrement part of Register)。cons cell 也就是 construction of cells。car 函数用于取得 cons cell 的 CAR 部分，cdr 取得cons cell 的 CDR 部分。cons cell 如此简单，但是它却能衍生出许多高级的数据结构，比如链表，树，关联表等等。

cons cell 的读入语法是用 . 分开两个部分，比如：

``` cl
'(1 . 2)                                ; => (1 . 2)
'(?a . 1)                               ; => (97 . 1)
'(1 . "a")                              ; => (1 . "a")
'(1 . nil)                              ; => (1)
'(nil . nil)                            ; => (nil)
```

注意到前面的表达式中都有一个 ' 号，这是什么意思呢？其实理解了 eval-last-sexp 的作用就能明白了。eval-last-sexp 其实包含了两个步骤，一是读入前一个 S-表达式，二是对读入的 S-表达式求值。这样如果读入的 S-表达式是一个 cons cell 的话，求值时会把这个 cons cell 的第一个元素作为一个函数来调用。而事实上，前面这些例子的第一个元素都不是一个函数，这样就会产生一个错误 invalid-function。之所以前面没有遇到这个问题，那是因为前面数字和字符串是一类特殊的 S-表达式，它们求值后和求值前是不变，称为自求值表达式（self-evaluating form）。' 号其实是一个特殊的函数 quote，它的作用是将它的参数返回而不作求值。'(1 . 2) 等价于 (quote (1 . 2))。为了证明 cons cell 的读入语法确实就是它的输出形式，可以看下面这个语句：

``` cl
(read "(1 . 2)")                        ; => (1 . 2)
```

列表包括了 cons cell。但是列表中有一个特殊的元素──空表 nil。

``` cl
nil                                     ; => nil
'()                                     ; => nil
```

空表不是一个 cons cell，因为它没有 CAR 和 CDR 两个部分，事实上空表里没有任何内容。但是为了编程的方便，可以认为 nil 的 CAR 和 CDR 都是 nil：

``` cl
(car nil)                               ; => nil
(cdr nil)                               ; => nil
```

按列表最后一个 cons cell 的 CDR 部分的类型分，可以把列表分为三类。如果它是 nil 的话，这个列表也称为“真列表”(true list)。如果既不是 nil 也不是一个 cons cell，则这个列表称为“点列表”(dotted list)。还有一种可能，它指向列表中之前的一个 cons cell，则称为环形列表(circular list)。这里分别给出一个例子：

``` cl
'(1 2 3)                                  ; => (1 2 3)
'(1 2 . 3)                                ; => (1 2 . 3)
'(1 . #1=(2 3 . #1#))                     ; => (1 2 3 . #1)
```

从这个例子可以看出前两种列表的读入语法和输出形式都是相同的，而环形列表的读入语法是很古怪的，输出形式不能作为环形列表的读入形式。

如果把真列表最后一个 cons cell 的 nil 省略不写，也就是 (1 . nil) 简写成 (1)，把 ( obj1 . ( obj2 . list)) 简写成 (obj1 obj2 . list)，那么列表最后可以写成一个用括号括起的元素列表：

``` cl
'(1 . (2 . (3 . nil)))                  ; => (1 2 3)
```

尽管这样写是清爽多了，但是，我觉得看一个列表时还是在脑子里反映的前面的形式，这样在和复杂的 cons cell 打交道时就不会搞不清楚这个 cons cell 的 CDR 是一个列表呢，还是一个元素或者是嵌套的列表。

## 测试函数 ##

测试一个对象是否是 cons cell 用 consp，是否是列表用 listp。

``` cl
(consp '(1 . 2))                        ; => t
(consp '(1 . (2 . nil)))                ; => t
(consp nil)                             ; => nil
(listp '(1 . 2))                        ; => t
(listp '(1 . (2 . nil)))                ; => t
(listp nil)                             ; => t
```

没有内建的方法测试一个列表是不是一个真列表。通常如果一个函数需要一个真列表作为参数，都是在运行时发出错误，而不是进行参数检查，因为检查一个列表是真列表的代价比较高。

测试一个对象是否是 nil 用 null 函数。只有当对象是空表时，null 才返回空值。

## 构造函数 ##

生成一个 cons cell 可以用 cons 函数。比如：

``` cl
(cons 1 2)                              ; => (1 . 2)
(cons 1 '())                            ; => (1)
```

也是在列表前面增加元素的方法。比如：

``` cl
(setq foo '(a b))                       ; => (a b)
(cons 'x foo)                           ; => (x a b)
```

值得注意的是前面这个例子的 foo 值并没有改变。事实上有一个宏 push 可以加入元素的同时改变列表的值：

``` cl
(push 'x foo)                           ; => (x a b)
foo                                     ; => (x a b)
```

生成一个列表的函数是 list。比如：

``` cl
(list 1 2 3)                            ; => (1 2 3)
```

可能这时你有一个疑惑，前面产生一个列表，我常用 quote（也就是 ' 符号）这个函数，它和这个 cons 和 list 函数有什么区别呢？其实区别是很明显的，quote 是把参数直接返回不进行求值，而 list 和 cons 是对参数求值后再生成一个列表或者 cons cell。看下面这个例子：

``` cl
'((+ 1 2) 3)                            ; => ((+ 1 2) 3)
(list (+ 1 2) 3)                        ; => (3 3)
```

前一个生成的列表的 CAR 部分是 (+ 1 2) 这个列表，而后一个是先对 (+ 1 2) 求值得到 3 后再生成列表。

> ### [思考题](#answer-list)
> 如果你觉得你有点明白的话，我提一个问题考考你：怎样用 list 函数构造一个 (a b c) 这样的列表呢？

前面提到在列表前端增加元素的方法是用 cons，在列表后端增加元素的函数是用 append。比如：

``` cl
(append '(a b) '(c))                    ; => (a b c)
```

append 的功能可以认为它是把第一个参数最后一个列表的 nil 换成第二个参数，比如前面这个例子，第一个参数写成 cons cell 表示方式是(a . (b . nil))，把这个 nil 替换成 (c) 就成了 (a . (b . (c)))。对于多个参数的情况也是一样的，依次把下一个参数替换新列表最后一个 nil 就是最后的结果了。

``` cl
(append '(a b) '(c) '(d))               ; => (a b c d)
```

一般来说 append 的参数都要是列表，但是最后一个参数可以不是一个列表，这也不违背前面说的，因为 cons cell 的 CDR 部分本来就可以是任何对象：

``` cl
(append '(a b) 'c)                      ; => (a b . c)
```

这样得到的结果就不再是一个真列表了，如果再进行 append 操作就会产生一个错误。

如果你写过 c 的链表类型，可能就知道如果链表只保留一个指针，那么链表只能在一端增加元素。elisp 的列表类型也是类似的，用 cons 在列表前增加元素比用 append 要快得多。

append 的参数不限于列表，还可以是字符串或者向量。前面字符串里已经提到可以把一个字符串转换成一个字符列表，同样可能把向量转换成一个列表：

``` cl
(append [a b] "cd" nil)                 ; => (a b 99 100)
```

注意前面最后一个参数 nil 是必要的，不然你可以想象得到的结果是什么。

## 把列表当数组用 ##
要得到列表或者 cons cell 里元素，唯一的方法是用 car 和 cdr 函数。很容易明白，car 就是取得 cons cell 的 CAR 部分，cdr 函数就是取得 cons cell 的 CDR 部分。通过这两个函数，我们就能访问 cons cell 和列表中的任何元素。

> ### [思考题](#answer-nthcdr)
> 你如果知道 elisp 的函数如果定义，并知道 if 的使用方法，不妨自己写一个函数来取得一个列表的第 n 个 CDR。

通过使用 elisp 提供的函数，我们事实上是可以把列表当数组来用。依惯例，我们用 car 来访问列表的第一个元素，cadr 来访问第二个元素，再往后就没有这样的函数了，可以用 nth 函数来访问：

``` cl
(nth 3 '(0 1 2 3 4 5))                  ; => 3
```

获得列表一个区间的函数有 nthcdr、last 和 butlast。nthcdr 和 last 比较类似，它们都是返回列表后端的列表。nthcdr 函数返回第 n 个元素后的列表：

``` cl
(nthcdr 2 '(0 1 2 3 4 5))               ; => (2 3 4 5)
```

last 函数返回倒数 n 个长度的列表：

``` cl
(last '(0 1 2 3 4 5) 2)                 ; => (4 5)
```

butlast 和前两个函数不同，返回的除了倒数 n 个元素的列表。

``` cl
(butlast '(0 1 2 3 4 5) 2)              ; => (0 1 2 3)
```

> ### [思考题](#answer-subseq)
> 如何得到某个区间（比如从3到5之间）的列表（提示列表长度可以用 length函数得到）：
>
> ``` cl
> (my-subseq '(0 1 2 3 4 5) 2 5)          ; => (2 3 4)
> ```

使用前面这几个函数访问列表是没有问题了。但是你也可以想象，链表这种数据结构是不适合随机访问的，代价比较高，如果你的代码中频繁使用这样的函数或者对一个很长的列表使用这样的函数，就应该考虑是不是应该用数组来实现。

直到现在为止，我们用到的函数都不会修改一个已有的变量。这是函数式编程的一个特点。只用这些函数编写的代码是很容易调试的，因为你不用去考虑一个变量在执行一个代码后就改变了，不用考虑变量的引用情况等等。下面就要结束这样轻松的学习了。

首先学习怎样修改一个 cons cell 的内容。首先 setcar 和 setcdr 可以修改一个 cons cell 的 CAR 部分和 CDR 部分。比如：

``` cl
(setq foo '(a b c))                     ; => (a b c)
(setcar foo 'x)                         ; => x
foo                                     ; => (x b c)
(setcdr foo '(y z))                     ; => (y z)
foo                                     ; => (x y z)
```

> ### [思考题](#answer-cirlist)
> 好像很简单是吧。我出一个比较 bt 的一个问题，下面代码运行后 foo 是什么东西呢？
>
> ``` cl
> (setq foo '(a b c))                     ; => (a b c)
> (setcdr foo foo)
> ```

现在来考虑一下，怎样像数组那样直接修改列表。使用 setcar 和 nthcdr 的组合就可以实现了：

``` cl
(setq foo '(1 2 3))                     ; => (1 2 3)
(setcar foo 'a)                         ; => a
(setcar (cdr foo) 'b)                   ; => b
(setcar (nthcdr 2 foo) 'c)              ; => c
foo                                     ; => (a b c)
```

## 把列表当堆栈用 ##
前面已经提到过可以用 push 向列表头端增加元素，在结合 pop 函数，列表就可以做为一个堆栈了。

``` cl
(setq foo nil)                          ; => nil
(push 'a foo)                           ; => (a)
(push 'b foo)                           ; => (b a)
(pop foo)                               ; => b
foo                                     ; => (a)
```

## 重排列表 ##

如果一直用 push 往列表里添加元素有一个问题是这样得到的列表和加入的顺序是相反的。通常我们需要得到一个反向的列表。reverse 函数可以做到这一点：

``` cl
(setq foo '(a b c))                     ; => (a b c)
(reverse foo)                           ; => (c b a)
```

需要注意的是使用 reverse 后 foo 值并没有改变。不要怪我太啰唆，如果你看到一个函数 nreverse，而且确实它能返回逆序的列表，不明所以就到处乱用，迟早会写出一个错误的函数。这个 nreverse 和前面的 reverse 差别就在于它是一个有破坏性的函数，也就是说它会修改它的参数。

``` cl
(nreverse foo)                          ; => (c b a)
foo                                     ; => (a)
```

为什么现在 foo 指向的是列表的末端呢？如果你实现过链表就知道，逆序操作是可以在原链表上进行的，这样原来头部指针会变成链表的尾端。列表也是（应该是，我也没有看过实现）这个原理。使用 nreverse 的唯一的好处是速度快，省资源。所以如果你只是想得到逆序后的列表就放心用 nreverse，否则还是用 reverse 的好。

elisp 还有一些是具有破坏性的函数。最常用的就是 sort 函数：

``` cl
(setq foo '(3 2 4 1 5))                 ; => (3 2 4 1 5)
(sort foo '<)                           ; => (1 2 3 4 5)
foo                                     ; => (3 4 5)
```

这一点请一定要记住，我就曾经在 sort 函数上犯了好几次错误。那如果我既要保留原列表，又要进行 sort 操作怎么办呢？可以用 copy-sequence 函数。这个函数只对列表进行复制，返回的列表的元素还是原列表里的元素，不会拷贝列表的元素。

nconc 和 append 功能相似，但是它会修改除最后一个参数以外的所有的参数，nbutlast 和 butlast 功能相似，也会修改参数。这些函数都是在效率优先时才使用。总而言之，以 n 开头的函数都要慎用。

## 把列表当集合用 ##

列表可以作为无序的集合。合并集合用 append 函数。去除重复的 equal 元素用 delete-dups。查找一个元素是否在列表中，如果测试函数是用 eq，就用 memq，如果测试用 equal，可以用 member。删除列表中的指定的元素，测试函数为 eq 对应 delq 函数，equal 对应 delete。还有两个函数 remq 和 remove 也是删除指定元素。它们的差别是 delq 和 delete 可能会修改参数，而 remq 和 remove 总是返回删除后列表的拷贝。注意前面这是说的是可能会修改参数的值，也就是说可能不会，所以保险起见，用 delq 和 delete 函数要么只用返回值，要么用 setq 设置参数的值为返回值。

``` cl
(setq foo '(a b c))                     ; => (a b c)
(remq 'b foo)                           ; => (a c)
foo                                     ; => (a b c)
(delq 'b foo)                           ; => (a c)
foo                                     ; => (a c)
(delq 'a foo)                           ; => (c)
foo                                     ; => (a c)
```

## 把列表当关联表 ##

用在 elisp 编程中，列表最常用的形式应该是作为一个关联表了。所谓关联表，就是可以用一个字符串（通常叫关键字，key）来查找对应值的数据结构。由列表实现的关联表有一个专门的名字叫 association list。尽管 elisp里也有 hash table，但是 hash table 相比于 association list 至少这样几个缺点：

 - hash table 里的关键字（key）是无序的，而 association list 的关键字
 可以按想要的顺序排列
 - hash table 没有列表那样丰富的函数，只有一个 maphash 函数可以遍历列
 表。而 association list 就是一个列表，所有列表函数都能适用
 - hash table 没有读入语法和输入形式，这对于调试和使用都带来很多不便

所以 elisp的hash table 不是一个首要的数据结构，只要不对效率要求很高，通常直接用association list。数组可以作为关联表，但是数组不适合作为与人交互使用数据结构（毕竟一个有意义的名字比纯数字的下标更适合人脑）。所以关联表的地位在 elisp 中就非比寻常了，emacs 为关联表专门用 c 程序实现了查找的相关函数以提高程序的效率。在 association list 中关键字是放在元素的 CAR 部分，与它对应的数据放在这个元素的 CDR 部分。根据比较方法的不同，有 assq 和assoc 两个函数，它们分别对应查找使用 eq 和 equal 两种方法。例如：

``` cl
(assoc "a" '(("a" 97) ("b" 98)))        ; => ("a" 97)
(assq 'a '((a . 97) (b . 98)))          ; => (a . 97)
```

通常我们只需要查找对应的数据，所以一般来说都要用 cdr 来得到对应的数据：

``` cl
(cdr (assoc "a" '(("a" 97) ("b" 98))))  ; => (97)
(cdr (assq 'a '((a . 97) (b . 98))))    ; => 97
```

assoc-default 可以一步完成这样的操作：

``` cl
(assoc-default "a" '(("a" 97) ("b" 98)))          ; => (97)
```

如果查找用的键值（key）对应的数据也可以作为一个键值的话，还可以用 rassoc 和 rassq 来根据数据查找键值：

``` cl
(rassoc '(97) '(("a" 97) ("b" 98)))     ; => ("a" 97)
(rassq '97 '((a . 97) (b . 98)))        ; => (a . 97)
```

如果要修改关键字对应的值，最省事的作法就是用 cons 把新的键值对加到列表的头端。但是这会让列表越来越长，浪费空间。如果要替换已经存在的值，一个想法就是用 setcdr 来更改键值对应的数据。但是在更改之前要先确定这个键值在对应的列表里，否则会产生一个错误。另一个想法是用 assoc 查找到对应的元素，再用 delq 删除这个数据，然后用 cons 加到列表里：

``` cl
(setq foo '(("a" . 97) ("b" . 98)))     ; => (("a" . 97) ("b" . 98))

;; update value by setcdr
(if (setq bar (assoc "a" foo))
    (setcdr bar "this is a")
  (setq foo (cons '("a" . "this is a") foo))) ; => "this is a"
foo                         ; => (("a" . "this is a") ("b" . 98))
;; update value by delq and cons
(setq foo (cons '("a" . 97)
                (delq (assoc "a" foo) foo))) ; => (("a" . 97) ("b" . 98))
```

如果不对顺序有要求的话，推荐用后一种方法吧。这样代码简洁，而且让最近更新的元素放到列表前端，查找更快。

## 把列表当树用 ##
列表的第一个元素如果作为结点的数据，其它元素看作是子节点，就是一个树了。由于树的操作都涉及递归，现在还没有说到函数，我就不介绍了。（其实是我不太熟，就不班门弄斧了）。

## 遍历列表 ##

遍历列表最常用的函数就是 mapc 和 mapcar 了。它们的第一个参数都是一个函数，这个函数只接受一个参数，每次处理一个列表里的元素。这两个函数唯一的差别是前者返回的还是输入的列表，而 mapcar 返回的函数返回值构成的列表：

``` cl
(mapc '1+ '(1 2 3))                     ; => (1 2 3)
(mapcar '1+ '(1 2 3))                   ; => (2 3 4)
```

另一个比较常用的遍历列表的方法是用 dolist。它的形式是：

``` cl
(dolist (var list [result]) body...)
```

其中 var 是一个临时变量，在 body 里可以用来得到列表中元素的值。使用 dolist 的好处是不用写lambda 函数。一般情况下它的返回值是 nil，但是你也可以指定一个值作为返回值（我觉得这个特性没有什么用，只省了一步而已）：

``` cl
(dolist (foo '(1 2 3))
  (incf foo))                           ; => nil
(setq bar nil)
(dolist (foo '(1 2 3) bar)
  (push (incf foo) bar))                ; => (4 3 2)
```

## 其它常用函数 ##

如果看过一些函数式语言教程的话，一定对 fold（或叫 accumulate、reduce）和 filter 这些函数记忆深刻。不过 elisp 里好像没有提供这样的函数。remove-if 和 remove-if-not 可以作 filter 函数，但是它们是 cl 里的，自己用用没有关系，不能强迫别人也跟着用，所以不能写到 elisp 里。如果不用这两个函数，也不用别人的函数的话，自己实现不妨用这样的方法：

``` cl
(defun my-remove-if (predicate list)
  (delq nil (mapcar (lambda (n)
                      (and (not (funcall predicate n)) n))
                    list)))
(defun evenp (n)
  (= (% n 2) 0))
(my-remove-if 'evenp '(0 1 2 3 4 5))    ; => (1 3 5)
```

fold 的操作只能用变量加循环或 mapc 操作来代替了：

``` cl
(defun my-fold-left (op initial list)
  (dolist (var list initial)
    (setq initial (funcall op initial var))))
(my-fold-left '+ 0 '(1 2 3 4))          ; => 10
```

这里只是举个例子，事实上你不必写这样的函数，直接用函数里的遍历操作更好一些。

产生数列常用的方法是用 number-sequence（这里不禁用说一次，不要再用 loop 产生 tab-stop-list 了，你们 too old 了）。不过这个函数好像 在emacs21 时好像还没有。

解析文本时一个很常用的操作是把字符串按分隔符分解，可以用 split-string 函数：

``` cl
(split-string "key = val" "\\s-*=\\s-*")  ; => ("key" "val")
```

与 split-string 对应是把几个字符串用一个分隔符连接起来，这可以用 mapconcat 完成。比如：

``` cl
(mapconcat 'identity '("a" "b" "c") "\t") ; => "a	b	c"
```

identity 是一个特殊的函数，它会直接返回参数。mapconcat 第一个参数是一个函数，可以很灵活的使用。

## 函数列表 ##

``` cl
;; 列表测试
(consp OBJECT)
(listp OBJECT)
(null OBJECT)
;; 列表构造
(cons CAR CDR)
(list &rest OBJECTS)
(append &rest SEQUENCES)
;; 访问列表元素
(car LIST)
(cdr LIST)
(cadr X)
(caar X)
(cddr X)
(cdar X)
(nth N LIST)
(nthcdr N LIST)
(last LIST &optional N)
(butlast LIST &optional N)
;; 修改 cons cell
(setcar CELL NEWCAR)
(setcdr CELL NEWCDR)
;; 列表操作
(push NEWELT LISTNAME)
(pop LISTNAME)
(reverse LIST)
(nreverse LIST)
(sort LIST PREDICATE)
(copy-sequence ARG)
(nconc &rest LISTS)
(nbutlast LIST &optional N)
;; 集合函数
(delete-dups LIST)
(memq ELT LIST)
(member ELT LIST)
(delq ELT LIST)
(delete ELT SEQ)
(remq ELT LIST)
(remove ELT SEQ)
;; 关联列表
(assoc KEY LIST)
(assq KEY LIST)
(assoc-default KEY ALIST &optional TEST DEFAULT)
(rassoc KEY LIST)
(rassq KEY LIST)
;; 遍历函数
(mapc FUNCTION SEQUENCE)
(mapcar FUNCTION SEQUENCE)
(dolist (VAR LIST [RESULT]) BODY...)
;; 其它
(number-sequence FROM &optional TO INC)
(split-string STRING &optional SEPARATORS OMIT-NULLS)
(mapconcat FUNCTION SEQUENCE SEPARATOR)
(identity ARG)
```

## 问题解答 ##

<a name="answer-list"></a>
#### 用 list 生成 (a b c) ####

答案是 `(list 'a 'b 'c)`。很简单的一个问题。从这个例子可以看出为什么要想出
用 ' 来输入列表。这就是程序员“懒”的美德呀！

<a name="answer-nthcdr"></a>
#### nthcdr 的一个实现 ####

``` cl
(defun my-nthcdr (n list)
  (if (or (null list) (= n 0))
      (car list)
    (my-nthcdr (1- n) (cdr list))))
```

这样的实现看上去很简洁，但是一个最大的问题的 elisp 的递归是有限的，所以如果想这个函数没有问题，还是用循环还实现比较好。

<a name="answer-subseq"></a>
#### my-subseq 函数的定义 ####

``` cl
(defun my-subseq (list from &optional to)
  (if (null to) (nthcdr from list)
    (butlast (nthcdr from list) (- (length list) to))))
```

<a name="answer-cirlist"></a>
#### (setcdr foo foo) 是什么怪东西？ ####

可能你已经想到了，这就是传说中的环呀。这在 info elisp - Circular Objects 里有介绍。elisp 里用到这样的环状列表并不多见，但是也不是没有，org 和 session 那个 bug 就是由于一个环状列表造成的。
