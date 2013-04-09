---
layout: page
title: 基本数据类型之二 ── 字符和字符串
---
在 emacs 里字符串是有序的字符数组。和 c 语言的字符串数组不同，emacs 的字符串可以容纳任何字符，包括 `\0`:

``` cl
(setq foo "abc\000abc")                 ; => "abc^@abc"
```

关于字符串有很多高级的属性，例如字符串的表示有单字节和多字节类型，字符串可以有文本属性（text property）等等。但是对于刚接触字符串，还是先学一些基本操作吧。

首先构成字符串的字符其实就是一个整数。一个字符 'A' 就是一个整数 65。但是目前字符串中的字符被限制在 0-524287 之间。字符的读入语法是在字符前加上一个问号，比如 ?A 代表字符 'A'。

``` cl
?A                                      ; => 65
?a                                      ; => 97
```

对于标点来说，也可以用同样的语法，但是最好在前面加上转义字符 \，因为有些标点会有岐义，比如 `?\(`。\ 必须用 ?\\\\ 表示。控制字符，退格、制表符，换行符，垂直制表符，换页符，空格，回车，删除和 escape 表示为 `?\a`, `?\b`, `?\t`, `?\n`, `?\v`, `?\f`, `?\s`, `?\r`, `?\d`, 和 `?\e`。对于没有特殊意义的字符，加上转义字符 \ 是没有副作用的，比如 `?\+` 和 `?+` 是完全一样的。所以标点还是都用转义字符来表示吧。

``` cl
?\a => 7                 ; control-g, `C-g'
?\b => 8                 ; backspace, <BS>, `C-h'
?\t => 9                 ; tab, <TAB>, `C-i'
?\n => 10                ; newline, `C-j'
?\v => 11                ; vertical tab, `C-k'
?\f => 12                ; formfeed character, `C-l'
?\r => 13                ; carriage return, <RET>, `C-m'
?\e => 27                ; escape character, <ESC>, `C-['
?\s => 32                ; space character, <SPC>
?\\ => 92                ; backslash character, `\'
?\d => 127               ; delete character, <DEL>
```

控制字符可以有多种表示方式，比如 `C-i`，这些都是对的：

``` cl
?\^I  ?\^i  ?\C-I  ?\C-i 
```

它们都对应数字 9。

meta 字符是用 <META> 修饰键（通常就是 Alt 键）输入的字符。之所以称为修饰键，是因为这样输入的字符就是在其修饰字符的第 27 位由 0 变成 1 而成，也就是如下操作：

``` cl
(logior (lsh 1 27) ?A)                  ; => 134217793
?\M-A                                   ; => 134217793
```

你可以用 `\M-` 代表 meta 键，加上修饰的字符就是新生成的字符。比如：`?\M-A`, `?\M-\C-b`. 后面这个也可以写成 `?\C-\M-b`。

如果你还记得前面说过字符串里的字符不能超过 524287 的话，这就可以看出字符串是不能放下一个 meta 字符的。所以按键序列在这时只能用 vector 来储存。

其它的修饰键也是类似的。emacs 用 `2**25` 位来表示 shift 键，`2**24` 对应 hyper，`2**23` 对应 super，`2**22` 对应 alt。

## 测试函数 ##

字符串测试使用 stringp，没有 charp，因为字符就是整数。 string-or-null-p 当对象是一个字符或 nil 时返回 t。char-or-string-p 测试是否是字符串或者字符类型。比较头疼的是 emacs 没有测试字符串是否为空的函数。这是我用的这个测试函数，使用前要测试字符串是否为 nil：

``` cl
(defun string-emptyp (str)
  (not (string< "" str)))
```

## 构造函数 ##

产生一个字符串可以用 make-string。这样生成的字符串包含的字符都是一样的。要生成不同的字符串可以用 string 函数。

``` cl
(make-string 5 ?x)                      ; => "xxxxx"
(string ?a ?b ?c)                       ; => "abc"
```

在已有的字符串生成新的字符串的方法有 substring, concat。substring 的后两个参数是起点和终点的位置。如果终点越界或者终点比起点小都会产生一个错误。这个在使用 substring 时要特别小心。

``` cl
(substring "0123456789" 3)              ; => "3456789"
(substring "0123456789" 3 5)            ; => "34"
(substring "0123456789" -3 -1)          ; => "78"
```

concat 函数相对简单，就是把几个字符串连接起来。

## 字符串比较 ##
char-equal 可以比较两个字符是否相等。与整数比较不同，这个函数还考虑了大小写。如果 case-fold-search 变量是 t 时，这个函数的字符比较是忽略大小写的。编程时要小心，因为通常 case-fold-search 都是 t，这样如果要考虑字符的大小写时就不能用 char-equal 函数了。

字符串比较使用 string=，string-equal 是一个别名。

string< 是按字典序比较两个字符串，string-less 是它的别名。空字符串小于所有字符串，除了空字符串。前面 string-emptyp 就是用这个特性。当然直接用 length 检测字符串长度应该也可以，还可以省去检测字符串是否为空。没有 string> 函数。

## 转换函数 ##

字符转换成字符串可以用 char-to-string 函数，字符串转换成字符可以用 string-to-char。当然只是返回字符串的第一个字符。

数字和字符串之间的转换可以用 number-to-string 和 string-to-number。其中 string-to-number 可以设置字符串的进制，可以从 2 到 16。number-to-string 只能转换成 10 进制的数字。如果要输出八进制或者十六进制，可以用 format 函数：

``` cl
(string-to-number "256")                ; => 256
(number-to-string 256)                  ; => "256"
(format "%#o" 256)                      ; => "0400"
(format "%#x" 256)                      ; => "0x100"
```

如果要输出成二进制，好像没有现成的函数了。calculator 库倒是可以，这是我写的函数：

``` cl
(defun number-to-bin-string (number)
  (require 'calculator)
  (let ((calculator-output-radix 'bin)
        (calculator-radix-grouping-mode nil))
    (calculator-number-to-string number)))
(number-to-bin-string 256)              ; => "100000000"
```

其它数据类型现在还没有学到，不过可以先了解一下吧。concat 可以把一个字符构成的列表或者向量转换成字符串，vconcat 可以把一个字符串转换成一个向量，append 可以把一个字符串转换成一个列表。

``` cl
(concat '(?a ?b ?c ?d ?e))              ; => "abcde"
(concat [?a ?b ?c ?d ?e])               ; => "abcde"
(vconcat "abdef")                       ; => [97 98 100 101 102]
(append "abcdef" nil)                   ; => (97 98 99 100 101 102)
```

大小写转换使用的是 downcase 和 upcase 两个函数。这两个函数的参数既可以字符串，也可以是字符。capitalize 可以使字符串中单词的第一个字符大写，其它字符小写。upcase-initials 只使第一个单词的第一个字符大写，其它字符小写。这两个函数的参数如果是一个字符，那么只让这个字符大写。比如：

``` cl
(downcase "The cat in the hat")         ; => "the cat in the hat"
(downcase ?X)                           ; => 120
(upcase "The cat in the hat")           ; => "THE CAT IN THE HAT"
(upcase ?x)                             ; => 88
(capitalize "The CAT in tHe hat")       ; => "The Cat In The Hat"
(upcase-initials "The CAT in the hAt")  ; => "The CAT In The HAt"
```

## 格式化字符串 ##

format 类似于 C 语言里的 printf 可以实现对象的字符串化。数字的格式化和 printf 的参数差不多，值得一提的是 "%S" 这个格式化形式，它可以把对象的输出形式转换成字符串，这在调试时是很有用的。

## 查找和替换 ##
字符串查找的核心函数是 string-match。这个函数可以从指定的位置对字符串进行正则表达式匹配，如果匹配成功，则返回匹配的起点，如：

``` cl
(string-match "34" "01234567890123456789")    ; => 3
(string-match "34" "01234567890123456789" 10) ; => 13
```

注意 string-match 的参数是一个 regexp。emacs 好象没有内建的查找子串的函数。如果你想把 string-match 作为一个查找子串的函数，可以先用 regexp-quote 函数先处理一下子串。比如：

``` cl
(string-match "2*" "232*3=696")                ; => 0
(string-match (regexp-quote "2*") "232*3=696") ; => 2
```

事实上，string-match 不只是查找字符串，它更重要的功能是捕捉匹配的字符串。如果你对正则表达式不了解，可能需要先找一本书，先了解一下什么是正则表达式。string-match 在查找的同时，还会记录下每个要捕捉的字符串的位置。这个位置可以在匹配后用 match-data、match-beginning 和 match-end 等函数来获得。先看一下例子：

``` cl
(progn
  (string-match "3\\(4\\)" "01234567890123456789")
  (match-data))                         ; => (3 5 4 5)
```

最后返回这个数字是什么意思呢？正则表达式捕捉的字符串按括号的顺序对应一个序号，整个模式对应序号 0，第一个括号对应序号 1，第二个括号对应序号 2，以此类推。所以 "3\\(4\\)" 这个正则表达式中有序号 0 和 1，最后 match-data 返回的一系列数字对应的分别是要捕捉字符串的起点和终点位置，也就是说子串 "34" 起点从位置 3 开始，到位置 5 结束，而捕捉的字符串 "4" 的起点是从 4 开始，到 5 结束。这些位置可以用 match-beginning 和 match-end 函数用对应的序号得到。要注意的是，起点位置是捕捉字符串的第一个字符的位置，而终点位置不是捕捉的字符串最后一个字符的位置，而是下一个字符的位置。这个性质对于循环是很方便的。比如要查找上面这个字符串中所有 34 出现的位置：

``` cl
(let ((start 0))
  (while (string-match "34" "01234567890123456789" start)
    (princ (format "find at %d\n" (match-beginning 0)))
    (setq start (match-end 0))))
```

查找会了，就要学习替换了。替换使用的函数是 replace-match。这个函数既可以用于字符串的替换，也可以用于缓冲区的文本替换。对于字符串的替换，replace-match 只是按给定的序号把字符串中的那一部分用提供的字符串替换了而已：

``` cl
(let ((str "01234567890123456789"))
  (string-match "34" str)
  (princ (replace-match "x" nil nil str 0))
  (princ "\n")
  (princ str))
```

可以看出 replace-match 返回的字符串是替换后的新字符串，原字符串被没有改变。

如果你想挑战一下，想想怎样把上面这个字符串中所有的 34 都替换掉？如果想就使用同一个字符串来存储，可能对于固定的字符串，这个还容易一些，如果不是的话，就要花一些脑筋了，因为替换之后，新的字符串下一个搜索起点的位置就不能用 (match-end 0) 给出来的位置了，而是要扣除替换的字符串和被替换的字符串长度的差值。

emacs 对字符串的替换有一个函数 replace-regexp-in-string。这个函数的实现方法是把每次匹配部分之前的子串收集起来，最后再把所有字符串连接起来。

单字符的替换有 subst-char-in-string 函数。但是 emacs 没有类似 perl函数或者程序 tr 那样进行字符替换的函数。只能自己建表进行循环操作了。

## 函数列表 ##

``` cl
;; 测试函数
(stringp OBJECT)
(string-or-null-p OBJECT)
(char-or-string-p OBJECT)
;; 构建函数
(make-string LENGTH INIT)
(string &rest CHARACTERS)
(substring STRING FROM &optional TO)
(concat &rest SEQUENCES)
;; 比较函数
(char-equal C1 C2)
(string= S1 S2)
(string-equal S1 S2)
(string< S1 S2)
;; 转换函数
(char-to-string CHAR)
(string-to-char STRING)
(number-to-string NUMBER)
(string-to-number STRING &optional BASE)
(downcase OBJ)
(upcase OBJ)
(capitalize OBJ)
(upcase-initials OBJ)
(format STRING &rest OBJECTS)
;; 查找与替换
(string-match REGEXP STRING &optional START)
(replace-match NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)
(replace-regexp-in-string REGEXP REP STRING &optional FIXEDCASE LITERAL SUBEXP START)
(subst-char-in-string FROMCHAR TOCHAR STRING &optional INPLACE)
```
