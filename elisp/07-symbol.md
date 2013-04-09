---
layout: page
title: 基本数据类型之五 ── 符号
---

符号是有名字的对象。可能这么说有点抽象。作个不恰当的比方，符号可以看作是 C 语言里的指针。通过符号你可以得到和这个符号相关联的信息，比如值，函数，属性列表等等。

首先必须知道的是符号的命名规则。符号名字可以含有任何字符。大多数的符号名字只含有字母、数字和标点“-+=*/”。这样的名字不需要其它标点。名字前缀要足够把符号名和数字区分开来，如果需要的话，可以在前面用 \ 表示为符号，比如：
{% highlight cl %}
(symbolp '+1)                           ; => nil
(symbolp '\+1)                          ; => t
(symbol-name '\+1)                      ; => "+1"
{% endhighlight %}

其它字符 `_~!@$%^&amp;:<>{}?` 用的比较少。但是也可以直接作为符号的名字。任何其它字符都可以用 \ 转义后用在符号名字里。但是和字符串里字符表示不同，\ 转义后只是表示其后的字符，比如 \t 代表的字符 t，而不是制表符。如果要在符号名里使用制表符，必须在 \ 后加上制表符本身。

符号名是区分大小写的。这里有一些符号名的例子：

     foo                 ; 名为 `foo' 的符号
     FOO                 ; 名为 `FOO' 的符号，和 `foo' 不同
     char-to-string      ; 名为 `char-to-string' 的符号
     1+                  ; 名为 `1+' 的符号 (不是整数 `+1')
     \+1                 ; 名为 `+1' 的符号 (可读性很差的名字)
     \(*\ 1\ 2\)         ; 名为 `(* 1 2)' 的符号 (更差劲的名字).
     +-*/_~!@$%^&=:<>{}  ; 名为 `+-*/_~!@$%^&=:<>{}' 的符号.
                         ;   这些字符无须转义

## 创建符号 ##

一个名字如何与数据对应上呢？这就要了解一下符号是如何创建的了。符号名要有唯一性，所以一定会有一个表与名字关联，这个表在 elisp 里称为 obarray。从这个名字可以看出这个表是用数组类型，事实上是一个向量。当 emacs 创建一个符号时，首先会对这个名字求 hash 值以得到一个在 obarray 这个向量中查找值所用的下标。hash 是查找字符串的很有效的方法。这里强调的是obarray 不是一个特殊的数据结构，就是一个一般的向量。全局变量 obarray里 emacs 所有变量、函数和其它符号所使用的 obarray（注意不同语境中obarray 的含义不同。前一个 obarray 是变量名，后一个 obarray 是数据类型名）。也可以自己建立向量，把这个向量作为 obarray 来使用。这是一种代替散列的一种方法。它比直接使用散列有这样一些好处：

 - 符号不仅可以有一个值，还可以用属性列表，后者又可以相当于一个关联列表。这样有很高的扩展性，而且可以表达更高级的数据结构。
 - emacs 里有一些函数可以接受 obarray 作为参数，比如补全相关的函数。

当 lisp 读入一个符号时，通常会先查找这个符号是否在 obarray 里出现过，如果没有则会把这个符号加入到 obarray 里。这样查找并加入一个符号的过程称为是 intern。intern 函数可以查找或加入一个名字到 obarray 里，返回对应的符号。默认是全局的obarray，也可以指定一个 obarray。intern-soft 与 intern 不同的是，当名字不在 obarray 里时，intern-soft 会返回 nil，而 intern 会加入到 obarray里。为了不污染 obarray，我下面的例子中尽量在 foo 这个 obarray 里进行。一般来说，去了 foo 参数，则会在 obarray 里进行。其结果应该是相同的：

{% highlight cl %}
(setq foo (make-vector 10 0))           ; => [0 0 0 0 0 0 0 0 0 0]
(intern-soft "abc" foo)                 ; => nil
(intern "abc" foo)                      ; => abc
(intern-soft "abc" foo)                 ; => abc
{% endhighlight %}

lisp 每读入一个符号都会 intern 到 obarray 里，如果想避免，可以用在符号名前加上 `#:`：

{% highlight cl %}
(intern-soft "abc")                     ; => nil
'abc                                    ; => abc
(intern-soft "abc")                     ; => abc
(intern-soft "abcd")                    ; => nil
'#:abcd                                 ; => abcd
(intern-soft "abcd")                    ; => nil
{% endhighlight %}

如果想除去 obarray 里的符号，可以用 unintern 函数。unintern 可以用符号名或符号作参数在指定的 obarray 里去除符号，成功去除则返回 t，如果没有查找到对应的符号则返回 nil：

{% highlight cl %}
(intern-soft "abc" foo)                 ; => abc
(unintern "abc" foo)                    ; => t
(intern-soft "abc" foo)                 ; => nil
{% endhighlight %}

和 hash-table 一样，obarray 也提供一个 mapatoms 函数来遍历整个 obarray。比如要计算 obarray 里所有的符号数量：

{% highlight cl %}
(setq count 0)                          ; => 0
(defun count-syms (s)
  (setq count (1+ count)))              ; => count-syms
(mapatoms 'count-syms)                  ; => nil
count                                   ; => 28371
(length obarray)                        ; => 1511
{% endhighlight %}

> ### [思考题](#answer-obarray)
> 由前面的例子可以看出elisp 中的向量长度都是有限的，而 obarray 里的符号有成千上万个。那这些符号是怎样放到 obarray 里的呢？

## 符号的组成 ##

每个符号可以对应四个组成部分，一是符号的名字，可以用 symbol-name 访问。二是符号的值。符号的值可以通过 set 函数来设置，用 symbol-value 来访问。

{% highlight cl %}
(set (intern "abc" foo) "I'm abc")      ; => "I'm abc"
(symbol-value (intern "abc" foo))       ; => "I'm abc"
{% endhighlight %}

可能大家最常见到 setq 函数，而 set 函数确很少见到。setq 可以看成是一个宏，它可以让你用 (setq sym val) 代替 (set (quote sym) val)。事实上这也是它名字的来源 (q 代表 quoted)。但是 setq 只能设置 obarray 里的变量，前面这个例子中就只能用 set 函数。

> ### [思考题](#answer-remove)
> 参考 assoc-default 的代码，写一个函数从一个关联列表中除去一个关键字对应的元素。这个函数可以直接修改关联列表符号的值。要求可以传递一个参数作为测试关键字是否相同的函数。比如：
>
> ``` cl
> (setq foo '((?a . a) (?A . c) (?B . d)))
> (remove-from-list 'foo ?b 'char-equal)  ; => ((97 . a) (65 . c))
> foo                                     ; => ((97 . a) (65 . c))
> ```

如果一个符号的值已经有设置过的话，则 boundp 测试返回 t，否则为 nil。对于 boundp 测试返回 nil 的符号，使用符号的值会引起一个变量值为 void 的错误。

符号的第三个组成部分是函数。它可以用 symbol-function 来访问，用 fset 来设置：
{% highlight cl %}
(fset (intern "abc" foo) (symbol-function 'car)) ; => #<subr car>
(funcall (intern "abc" foo) '(a . b))            ; => a
{% endhighlight %}

类似的，可以用 fboundp 测试一个符号的函数部分是否有设置。

符号的第四个组成部分是属性列表(property list)。通常属性列表用于存储和符号相关的信息，比如变量和函数的文档，定义的文件名和位置，语法类型。属性名和值可以是任意的 lisp 对象，但是通常名字是符号，可以用 get 和 put 来访问和修改属性值，用 symbol-plist 得到所有的属性列表：

{% highlight cl %}
(put (intern "abc" foo) 'doc "this is abc")      ; => "this is abc"
(get (intern "abc" foo) 'doc)                    ; => "this is abc"
(symbol-plist (intern "abc" foo))                ; => (doc "this is abc")
{% endhighlight %}

关联列表和属性列表很相似。符号的属性列表在内部表示上是用(prop1 value1 prop2 value2 ...) 的形式，和关联列表也是很相似的。属性列表在查找和这个符号相关的信息时，要比直接用关联列表要简单快捷的多。所以变量的文档等信息都是放在符号的属性列表里。但是关联表在头端加入元素是很快的，而且它可以删除表里的元素。而属性列表则不能删除一个属性。

如果已经把属性列表取出，那么还可以用 plist-get 和 plist-put 的方法来访问和设置属性列表：
{% highlight cl %}
(plist-get '(foo 4) 'foo)               ; => 4
(plist-get '(foo 4 bad) 'bar)           ; => nil
(setq my-plist '(bar t foo 4))          ; => (bar t foo 4)
(setq my-plist (plist-put my-plist 'foo 69)) ; => (bar t foo 69)
(setq my-plist (plist-put my-plist 'quux '(a))) ; => (bar t foo 69 quux (a))
{% endhighlight %}

> ### [思考题](#answer-plist)
> 你能不能用已经学过的函数来实现 plist-get 和 plist-put？

## 函数列表 ##
{% highlight cl %}
(symbolp OBJECT)
(intern-soft NAME &optional OBARRAY)
(intern STRING &optional OBARRAY)
(unintern NAME &optional OBARRAY)
(mapatoms FUNCTION &optional OBARRAY)
(symbol-name SYMBOL)
(symbol-value SYMBOL)
(boundp SYMBOL)
(set SYMBOL NEWVAL)
(setq SYM VAL SYM VAL ...)
(symbol-function SYMBOL)
(fset SYMBOL DEFINITION)
(fboundp SYMBOL)
(symbol-plist SYMBOL)
(get SYMBOL PROPNAME)
(put SYMBOL PROPNAME VALUE)
{% endhighlight %}

## 问题解答 ##

<a name="answer-obarray"></a>
#### obarray 里符号数为什么大于向量长度 ####
其实这和散列的的实现是一样的。obarray 里的每一个元素通常称为 bucket。
一个 bucket 是可以容纳多个相同 hash 值的字符串和它们的数据。我们可以用
这样的方法来模拟一下：

{% highlight cl %}
(defun hash-string (str)
  (let ((hash 0) c)
    (dotimes (i (length str))
      (setq c (aref str i))
      (if (> c #o140)
          (setq c (- c 40)))
      (setq hash (+ (setq hash (lsh hash 3))
                    (lsh hash -28)
                    c)))
    hash))

(let ((len 10) str hash)
  (setq foo (make-vector len 0))
  (dotimes (i (1+ len))
    (setq str (char-to-string (+ ?a i))
          hash (% (hash-string str) len))
    (message "I put %s in slot %d"
             str hash)
    (if (eq (aref foo hash) 0)
        (intern str foo)
      (message "I found %S is already taking the slot: %S"
               (aref foo hash) foo)
      (intern str foo)
      (message "Now I'am in the slot too: %S" foo))))
{% endhighlight %}

在我这里的输出是：
{% highlight cl %}
I put a in slot 7
I put b in slot 8
I put c in slot 9
I put d in slot 0
I put e in slot 1
I put f in slot 2
I put g in slot 3
I put h in slot 4
I put i in slot 5
I put j in slot 6
I put k in slot 7
I found a is already taking the slot: [d e f g h i j a b c]
Now I'am in the slot too: [d e f g h i j k b c]
{% endhighlight %}

当然，这个 hash-string 和实际 obarray 里用的 hash-string 只是算法上是
相同的，但是由于数据类型和 c 不是完全相同，所以对于长一点的字符串结果
可能不一样，我只好用单个字符来演示一下。

<a name="answer-remove"></a>
#### 根据关键字删除关联列表中的元素 ####
{% highlight cl %}
(defun remove-from-list (list-var key &optional test)
  (let ((prev (symbol-value list-var))
        tail found value elt)
    (or test (setq test 'equal))
    (if (funcall test (caar prev) key)
        (set list-var (cdr prev))
      (setq tail (cdr prev))
      (while (and tail (not found))
        (setq elt (car tail))
        (if (funcall test (car elt) key)
            (progn
              (setq found t)
              (setcdr prev (cdr tail)))
          (setq tail (cdr tail)
                prev (cdr prev)))))
    (symbol-value list-var)))
{% endhighlight %}

注意这个函数的参数 list-var 是一个符号，所以这个函数不能直接传递一个列表。这和 add-to-list 的参数是一样的。

<a name="answer-plist"></a>
#### plist-get 和 plist-put 的实现 ####

{% highlight cl %}
(defun my-plist-get (plist prop)
  (cadr (memq plist prop)))
(defun my-plist-put (plist prop val)
  (let ((tail (memq prop plist)))
    (if tail
        (setcar (cdr tail) val)
      (setcdr (last plist) (list prop val))))
  plist)
{% endhighlight %}

my-plist-put 函数没有 plist-put 那样 robust，如果属性列表是 '(bar t foo) 这样的话，这个函数就会出错。而且加入一个属性的时间复杂度比 plist 更高（memq 和 last 都是 O(n)），不过可以用循环来达到相同的时间复杂度。


