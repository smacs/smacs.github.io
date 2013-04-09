---
layout: page
title: 一个 Hello World 例子
---

自从 K&R 以来，hello world 程序历来都是程序语言教程的第一个例子。我也用一个 hello world 的例子来演示 emacs 里执行 elisp 的环境。下面就是这个语句：

{% highlight cl %}
(message "hello world")
{% endhighlight %}

前面我没有说这个一个程序，这是因为，elisp 不好作为可执行方式来运行（当然也不是不可能），所有的 elisp 都是运行在 emacs 这个环境下。

首先切换到 `*scratch*` 缓冲区里，如果当前模式不是 `lisp-interaction-mode`，用 `M-x lisp-interaction-mode` 先转换到 `lisp-interaction-mode`。然后输入前面这一行语句。在行尾右括号后，按 `C-j` 键。如果 `Minibuffer` 里显示 `hello world`，光标前一行也显示 `"hello world"`，那说明你的操作没有问题。我们就可以开始 elisp 学习之旅了。

注：elisp 里的一个完整表达式，除了简单数据类型（如数字，向量），都是用括号括起来，称为一个 S-表达式。让 elisp 解释器执行一个 S-表达式除了前一种方法之外，还可以用 `C-x C-e`。它们的区别是，`C-x C-e` 是一个全局按键绑定，几乎可以在所有地方都能用。它会将运行返回值显示在 `Minibuffer` 里。这里需要强调一个概念是返回值和作用是不同的。比如前面 `message` 函数它的作用是在 Minibuffer 里显示一个字符串，但是它的返回值是 `"hello world"` 字符串。


