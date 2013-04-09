---
layout: page
title: 函数和命令
---

在 elisp 里类似函数的对象很多，比如：

 - 函数。这里的函数特指用 lisp 写的函数。
 - 原子函数（primitive）。用 C 写的函数，比如 car、append。
 - lambda 表达式
 - 特殊表达式
 - 宏(macro)。宏是用 lisp 写的一种结构，它可以把一种 lisp 表达式转换成等价的另一个表达式。
 - 命令。命令能用 command-execute 调用。函数也可以是命令。

以上这些用 functionp 来测试都会返回 t。

我们已经学过如何定义一个函数。但是这些函数的参数个数都是确定。但是你可以看到 emacs 里有很多函数是接受可选参数，比如 random 函数。还有一些函数可以接受不确定的参数，比如加减乘除。这样的函数在 elisp 中是如何定义的呢？

## 参数列表的语法 ##

这是参数列表的方法形式：

     (REQUIRED-VARS...
      [&optional OPTIONAL-VARS...]
      [&rest REST-VAR])

它的意思是说，你必须把必须提供的参数写在前面，可选的参数写在后面，最后用一个符号表示剩余的所有参数。比如：
{% highlight cl %}
(defun foo (var1 var2 &optional opt1 opt2 &rest rest)
  (list var1 var2 opt1 opt2 rest))

(foo 1 2)                               ; => (1 2 nil nil nil)
(foo 1 2 3)                             ; => (1 2 3 nil nil)
(foo 1 2 3 4 5 6)                       ; => (1 2 3 4 (5 6))
{% endhighlight %}

从这个例子可以看出，当可选参数没有提供时，在函数体里，对应的参数值都是 nil。同样调用函数时没有提供剩余参数时，其值也为 nil，但是一旦提供了剩余参数，则所有参数是以列表的形式放在对应变量里。

> ### [思考题](#answer-arg)
> 写一个函数测试两个浮点数是否相等，设置一个可选参数，如果提供这个参数，则用这个参数作为测试误差，否则用 1.0e-6 作为误差。

## 关于文档字符串 ##

最好为你的函数都提供一个文档字符串。关于文档字符串有一些规范，最好遵守这些约定。

字符串的第一行最好是独立的。因为 apropos 命令只能显示第一行的文档。所以最好用一行（一两个完整的句子）总结这个函数的目的。

文档的缩进最好要根据最后的显示的效果来调用。因为引号之类字符会多占用一个字符，所以在源文件里缩进最好看，不一定显示的最好。

如果你想要让你的函数参数显示的与函数定义的不同（比如提示用户如何调用这个函数），可以在文档最后一行，加上一行：

    \(fn ARGLIST)

注意这一行前面要有一个空行，这一行后不能再有空行。比如：
{% highlight cl %}
(defun foo (var1 var2 &optional opt1 opt2 &rest rest)
  "You should call the function like:

\(fn v1 v2)"
  (list var1 var2 opt1 opt2 rest))
{% endhighlight %}

还有一些有特殊标记功能的符号，比如 ``'` 引起的符号名可以生成一个链接，这样可以在 `*Help*` 中更方便的查看相关变量或函数的文档。`\\{major-mode-map}` 可以显示扩展成这个模式按键的说明，例如：

{% highlight cl %}
(defun foo ()
  "A simple document string to show how to use `' and \\=\\{}.
You can press this button `help' to see the document of
function \"help\".

This is keybind of text-mode(substitute from \\=\\{text-mode-map}):
\\{text-mode-map}

See also `substitute-command-keys' and `documentation'"
  )
{% endhighlight %}

## 调用函数 ##
通常函数的调用都是用 eval 进行的，但是有时需要在运行时才决定使用什么函数，这时就需要用 funcall 和 apply 两个函数了。这两个函数都是把其余的参数作为函数的参数进行调用。那这两个函数有什么参数呢？唯一的区别就在于 funcall 是直接把参数传递给函数，而 apply 的最后一个参数是一个列表，传入函数的参数把列表进行一次平铺后再传给函数，看下面这个例子就明白了：
{% highlight cl %}
(funcall 'list 'x '(y) '(z))               ; => (x (y) (z))
(apply 'list 'x '(y ) '(z))                ; => (x (y) z)
{% endhighlight %}

> ### [思考题](#answer-apply)
> 如果一个 list 作为一个树的结构，任何是 cons cell 的元素都是一个内部节点（不允许有 dotted list 出现），任何不是 cons cell 的元素都是树的叶子。请写一个函数，调用的一个类似 mapcar 的函数，调用一个函数遍历树的叶子，并收集所有的结果，返回一个结构相同的树，比如：
>
> ``` cl
> (tree-mapcar '1+ '(1 (2 (3 4)) (5)))    ; => (2 (3 (4 5)) (6))
> ```

## 宏 ##
前面在已经简单介绍过宏。宏的调用和函数是很类似的，它的求值和函数差不多，但是有一个重要的区别是，宏的参数是出现在最后扩展后的表达式中，而函数参数是求值后才传递给这个函数：

{% highlight cl %}
(defmacro foo (arg)
  (list 'message "%d %d" arg arg))

(defun bar (arg)
  (message "%d %d" arg arg))

(let ((i 1))
  (bar (incf i)))                       ; => "2 2"

(let ((i 1))
  (foo (incf i)))                       ; => "2 3"
{% endhighlight %}

也许你对前面这个例子 foo 里为什么要用 list 函数很不解。其实宏可以这样看，如果把宏定义作一个表达式来运行，最后把参数用调用时的参数替换，这样就得到了宏调用最后用于求值的表达式。这个过程称为扩展。可以用 macroexpand 函数进行模拟：
{% highlight cl %}
(macroexpand '(foo (incf i))) ; => (message "%d %d" (incf i) (incf i))
{% endhighlight %}

上面用 macroexpand 得到的结果就是用于求值的表达式。

使用 macroexpand 可以使宏的编写变得容易一些。但是如果不能进行 debug 是很不方便的。在宏定义里可以引入 declare 表达式，它可以增加一些信息。目前只支持两类声明：debug 和 indent。debug 可选择的类型很多，具体参考 info elisp - Edebug 一章，一般情况下用 t 就足够了。indent 的类型比较简单，它可以使用这样几种类型：

 - nil 也就是一般的方式缩进
 - defun 类似 def 的结构，把第二行作为主体，对主体里的表达式使用同样的缩进
 - 整数 表示从第 n 个表达式后作为主体。比如 if 设置为 2，而 when 设置为 1
 - 符号 这个是最坏情况，你要写一个函数自己处理缩进。

看 when 的定义就能知道 declare 如何使用了：
{% highlight cl %}
(defmacro when (cond &rest body)
  (declare (indent 1) (debug t))
  (list 'if cond (cons 'progn body)))
{% endhighlight %}

实际上，declare 声明只是设置这个符号的属性列表：
{% highlight cl %}
(symbol-plist 'when)    ; => (lisp-indent-function 1 edebug-form-spec t)
{% endhighlight %}

> ### [思考题](#answer-declare)
> 一个比较常用的结构是当 buffer 是可读情况下，绑定 inhibit-read-only 值为 t 来强制插入字符串。请写一个这样的宏，处理好缩进和调用。

从前面宏 when 的定义可以看出直接使用 list，cons，append 构造宏是很麻烦的。为了使记号简洁，lisp 中有一个特殊的宏 "`"，称为 backquote。在这个宏里，所有的表达式都是引起（quote）的，如果要让一个表达式不引起（也就是列表中使用的是表达式的值），需要在前面加 “,”，如果要让一个列表作为整个列表的一部分（slice），可以用 ",@"。

{% highlight cl %}
`(a list of ,(+ 2 3) elements)          ; => (a list of 5 elements)
(setq some-list '(2 3))                 ; => (2 3)
`(1 ,some-list 4 ,@some-list)           ; => (1 (2 3) 4 2 3)
{% endhighlight %}

有了这些标记，前面 when 这个宏可以写成：
{% highlight cl %}
(defmacro when (cond &rest body)
  `(if ,cond
       (progn ,@body)))
{% endhighlight %}

值得注意的是这个 backquote 本身就是一个宏，从这里可以看出宏除了减少重复代码这个作用之外的另一个用途：定义新的控制结构，甚至增加新的语法特性。

## 命令 ##

emacs 运行时就是处于一个命令循环中，不断从用户那得到按键序列，然后调用对应命令来执行。lisp 编写的命令都含有一个 interactive 表达式。这个表达式指明了这个命令的参数。比如下面这个命令：
{% highlight cl %}
(defun hello-world (name)
  (interactive "sWhat you name? ")
  (message "Hello, %s" name))
{% endhighlight %}

现在你可以用 M-x 来调用这个命令。让我们来看看 interactive 的参数是什么意思。这个字符串的第一个字符（也称为代码字符）代表参数的类型，比如这里 s 代表参数的类型是一个字符串，而其后的字符串是用来提示的字符串。如果这个命令有多个参数，可以在这个提示字符串后使用换行符分开，比如：

{% highlight cl %}
(defun hello-world (name time)
  (interactive "sWhat you name? \nnWhat the time? ")
  (message "Good %s, %s"
           (cond ((< time 13) "morning")
                 ((< time 19) "afternoon")
                 (t "evening"))
           name))
{% endhighlight %}

interactive 可以使用的代码字符很多，虽然有一定的规则，比如字符串用 s，数字用 n，文件用 f，区域用 r，但是还是很容易忘记，用的时候看 interactive 函数的文档还是很有必要的。但是不是所有时候都参数类型都能使用代码字符，而且一个好的命令，应该尽可能的让提供默认参数以让用户少花时间在输入参数上，这时，就有可能要自己定制参数。

首先学习和代码字符等价的几个函数。s 对应的函数是 read-string。比如：
{% highlight cl %}
(read-string "What your name? " user-full-name)
{% endhighlight %}

n 对应的函数是 read-number，文件对应 read-file-name。很容易记对吧。其实大部分代码字符都是有这样对应的函数或替换的方法（见下表）。


代码字符 | 代替的表达式
---------|----------------------------------------------------
a        |(completing-read prompt obarray 'fboundp t)
b        |(read-buffer prompt nil t)
B        |(read-buffer prompt)
c        |(read-char prompt)
C        |(read-command prompt)
d        |(point)
D        |(read-directory-name prompt)
e        |(read-event)
f        |(read-file-name prompt nil nil t)
F        |(read-file-name prompt)
G        |暂时不知道和 f 的差别
k        |(read-key-sequence prompt)
K        |(read-key-sequence prompt nil t)
m        |(mark)
n        |(read-number prompt)
N        |(if current-prefix-arg (prefix-numeric-value current-prefix-arg) (read-number prompt))
p        |(prefix-numeric-value current-prefix-arg)
P        |current-prefix-arg
r        |(region-beginning) (region-end)
s        |(read-string prompt)
S        |(completing-read prompt obarray nil t)
v        |(read-variable prompt)
x        |(read-from-minibuffer prompt nil nil t)
X        |(eval (read-from-minibuffer prompt nil nil t))
z        |(read-coding-system prompt)
Z        |(and current-prefix-arg (read-coding-system prompt))

知道这些表达式如何用于 interactive 表达式里呢？简而言之，如果 interactive 的参数是一个表达式，则这个表达式求值后的列表元素对应于这个命令的参数。请看这个例子：

{% highlight cl %}
(defun read-hiden-file (file arg)
  (interactive
   (list (read-file-name "Choose a hiden file: " "~/" nil nil nil
                         (lambda (name)
                           (string-match "^\\." (file-name-nondirectory name))))
         current-prefix-arg))
  (message "%s, %S" file arg))
{% endhighlight %}

第一个参数是读入一个以 "." 开头的文件名，第二个参数为当前的前缀参数（prefix argument），它可以用 C-u 或 C-u 加数字提供。list 把这两个参数构成一个列表。这就是命令一般的自定义设定参数的方法。

需要注意的是 current-prefix-arg 这个变量。这个变量当一个命令被调用，它就被赋与一个值，你可以用 C-u 就能改变它的值。在命令运行过程中，它的值始终都存在。即使你的命令不用参数，你也可以访问它：
{% highlight cl %}
(defun foo ()
  (interactive)
  (message "%S" current-prefix-arg))
{% endhighlight %}

用 C-u foo 调用它，你可以发现它的值是 (4)。那为什么大多数命令还单独为它设置一个参数呢？这是因为命令不仅是用户可以调用，很可能其它函数也可以调用，单独设置一个参数可以方便的用参数传递的方法调用这个命令。事实上所有的命令都可以不带参数，而使用前面介绍的方法在命令定义的部分读入需要的参数，但是为了提高命令的可重用性和代码的可读性，还是把参数分离到 interactive 表达式里好。

从现在开始可能会遇到很多函数，它们的用法有的简单，有的却复杂的要用大段篇幅来解释。我可能就会根据需要来解释一两个函数，就不一一介绍了。自己看 info elisp，用 i 来查找对应的函数。

> ### [思考题](#answer-switch-mode)
>
> 写一个命令用来切换 major-mode。要求用户输入一个 major-mode 的名字，就切换到这个 major-mode，而且要提供一种补全的办法，去除所有不是 major-mode 的符号，这样用户需要输入少量词就能找到对应的 major-mode。

## 函数列表 ##
{% highlight cl %}
(functionp OBJECT)
(apply FUNCTION &rest ARGUMENTS)
(funcall FUNCTION &rest ARGUMENTS)
(defmacro NAME ARGLIST [DOCSTRING] [DECL] BODY...)
(macroexpand FORM &optional ENVIRONMENT)
(declare &rest SPECS)
(` ARG)
(interactive ARGS)
(read-string PROMPT &optional INITIAL-INPUT HISTORY DEFAULT-VALUE
             INHERIT-INPUT-METHOD)
(read-file-name PROMPT &optional DIR DEFAULT-FILENAME MUSTMATCH
                INITIAL PREDICATE)
(completing-read PROMPT COLLECTION &optional PREDICATE
                 REQUIRE-MATCH INITIAL-INPUT HIST DEF
                 INHERIT-INPUT-METHOD)
(read-buffer PROMPT &optional DEF REQUIRE-MATCH)
(read-char &optional PROMPT INHERIT-INPUT-METHOD SECONDS)
(read-command PROMPT &optional DEFAULT-VALUE)
(read-directory-name PROMPT &optional DIR DEFAULT-DIRNAME
                     MUSTMATCH INITIAL)
(read-event &optional PROMPT INHERIT-INPUT-METHOD SECONDS)
(read-key-sequence PROMPT &optional CONTINUE-ECHO
                   DONT-DOWNCASE-LAST CAN-RETURN-SWITCH-FRAME
                   COMMAND-LOOP)
(read-number PROMPT &optional DEFAULT)
(prefix-numeric-value RAW)
(read-from-minibuffer PROMPT &optional INITIAL-CONTENTS KEYMAP
                      READ HIST DEFAULT-VALUE INHERIT-INPUT-METHOD)
(read-coding-system PROMPT &optional DEFAULT-CODING-SYSTEM)
{% endhighlight %}

## 变量列表 ##

{% highlight cl %}
current-prefix-arg
{% endhighlight %}

## 问题解答 ##

<a name="answer-arg"></a>
#### 可选误差的浮点数比较 ####
{% highlight cl %}
(defun approx-equal (x y &optional err)
  (if err
      (setq err (abs err))
    (setq err 1.0e-6))
  (or (and (= x 0) (= y 0))
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         err)))
{% endhighlight %}

这个应该是很简单的一个问题。

<a name="answer-apply"></a>
#### 遍历树的函数 ####
{% highlight cl %}
(defun tree-mapcar (func tree)
  (if (consp tree)
      (mapcar (lambda (child)
                (tree-mapcar func child))
              tree)
    (funcall func tree)))
{% endhighlight %}

这个函数可能对于树算法比较熟悉的人一点都不难，就当练手吧。

<a name="answer-declare"></a>
#### 宏 with-inhibit-read-only-t ####
{% highlight cl %}
(defmacro with-inhibit-read-only-t (&rest body)
  (declare (indent 0) (debug t))
  (cons 'let (cons '((inhibit-read-only t))
                   body)))
{% endhighlight %}

如果用 backquote 来改写一个就会发现这个宏会很容易写，而且更容易读了。

<a name="answer-switch-mode"></a>
#### 切换 major-mode 的命令 ####
{% highlight cl %}
(defvar switch-major-mode-history nil)
(defun switch-major-mode (mode)
  (interactive
   (list
    (intern
     (completing-read "Switch to mode: "
                      obarray (lambda (s)
                                (and (fboundp s)
                                     (string-match "-mode$" (symbol-name s))))
                      t nil 'switch-major-mode-history))))
  (setq switch-major-mode-history
        (cons (symbol-name major-mode) switch-major-mode-history))
  (funcall mode))
{% endhighlight %}

这是我常用的一个命令之一。这个实现也是一个使用 minibuffer 历史的例子。


