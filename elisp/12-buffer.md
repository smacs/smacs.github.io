---
layout: page
title: 操作对象之一 ── 缓冲区
---

缓冲区（buffer）是用来保存要编辑文本的对象。通常缓冲区都是和文件相关联的，但是也有很多缓冲区没有对应的文件。emacs 可以同时打开多个文件，也就是说能同时有很多个缓冲区存在，但是在任何时候都只有一个缓冲区称为当前缓冲区（current buffer）。即使在 lisp 编程中也是如此。许多编辑和移动的命令只能针对当前缓冲区。

## 缓冲区的名字 ##

emacs 里的所有缓冲区都有一个不重复的名字。所以和缓冲区相关的函数通常都是可以接受一个缓冲区对象或一个字符串作为缓冲区名查找对应的缓冲区。下面的函数列表中如果参数是 BUFFER-OR-NAME 则是能同时接受缓冲区对象和缓冲区名的函数，否则只能接受一种参数。有一个习惯是名字以空格开头的缓冲区是临时的，用户不需要关心的缓冲区。所以现在一般显示缓冲区列表的命令都不会显示这样的变量，除非这个缓冲区关联一个文件。

要得到缓冲区的名字，可以用 buffer-name 函数，它的参数是可选的，如果不指定参数，则返回当前缓冲区的名字，否则返回指定缓冲区的名字。更改一个缓冲区的名字用 rename-buffer，这是一个命令，所以你可以用 M-x 调用来修改当前缓冲区的名字。如果你指定的名字与现有的缓冲区冲突，则会产生一个错误，除非你使用第二个可选参数以产生一个不相同的名字，通常是在名字后加上 `<序号>` 的方式使名字变得不同。你也可以用 generate-new-buffer-name 来产生一个唯一的缓冲区名。

## 当前缓冲区 ##

当前缓冲区可以用 current-buffer 函数得到。当前缓冲区不一定是显示在屏幕上的那个缓冲区，你可以用 set-buffer 来指定当前缓冲区。但是需要注意的是，当命令返回到命令循环时，光标所在的缓冲区
会自动成为当前缓冲区。这也是单独在 `*scratch*` 中执行 set-buffer 后并不能改变当前缓冲区，而必须使用 progn 语句同时执行多个语句才能改变当前缓冲区的原因：
{% highlight cl %}
(set-buffer "*Messages*")   ; => #<buffer *Messages*>
(message (buffer-name))                  ; => "*scratch*"
(progn
  (set-buffer "*Messages*")
  (message (buffer-name)))               ; "*Messages*"
{% endhighlight %}

但是你不能依赖命令循环来把当前缓冲区设置成使用 set-buffer 之前的。因为这个命令很可以会被另一个程序员来调用。你也不能直接用 set-buffer 设置成原来的缓冲区，比如：
{% highlight cl %}
(let (buffer-read-only
      (obuf (current-buffer)))
  (set-buffer ...)
  ...
  (set-buffer obuf))
{% endhighlight %}

因为 set-buffer 不能处理错误或退出情况。正确的作法是使用 save-current-buffer、with-current-buffer 和 save-excursion 等方法。save-current-buffer 能保存当前缓冲区，执行其中的表达式，最后恢复为原来的缓冲区。如果原来的缓冲区被关闭了，则使用最后使用的那个当前缓冲区作为语句返回后的当前缓冲区。lisp 中很多以 with 开头的宏，这些宏通常是在不改变当前状态下，临时用另一个变量代替现有变量执行语句。比如 with-current-buffer 使用另一个缓冲区作为当前缓冲区，语句执行结束后恢复成执行之前的那个缓冲区。
{% highlight cl %}
(with-current-buffer BUFFER-OR-NAME
   body)
{% endhighlight %}

相当于：
{% highlight cl %}
(save-current-buffer
  (set-buffer BUFFER-OR-NAME)
  body)
{% endhighlight %}

save-excursion 与 save-current-buffer 不同之处在于，它不仅保存当前缓冲区，还保存了当前的位置和 mark。在 `*scratch*` 缓冲区中运行下面两个语句就能看出它们的差别了：
{% highlight cl %}
(save-current-buffer
  (set-buffer "*scratch*")
  (goto-char (point-min))
  (set-buffer "*Messages*"))

(save-excursion
  (set-buffer "*scratch*")
  (goto-char (point-min))
  (set-buffer "*Messages*"))
{% endhighlight %}

## 创建和关闭缓冲区 ##

产生一个缓冲区必须用给这个缓冲区一个名字，所以两个能产生新缓冲区的函数都是以一个字符串为参数：get-buffer-create 和 generate-new-buffer。这两个函数的差别在于前者如果给定名字的缓冲区已经存在，则返回这个缓冲区对象，否则新建一个缓冲区，名字为参数字符串，而后者在给定名字的缓冲区存在时，会使用加上后缀 `<N>`（N 是一个整数，从2开始） 的名字创建新的缓冲区。

关闭一个缓冲区可以用 kill-buffer。当关闭缓冲区时，如果要用户确认是否要关闭缓冲区，可以加到 kill-buffer-query-functions 里。如果要做一些善后处理，可以用 kill-buffer-hook。

通常一个接受缓冲区作为参数的函数都需要参数所指定的缓冲区是存在的。如果要确认一个缓冲区是否依然还存在可以使用 buffer-live-p。

要对所有缓冲区进行某个操作，可以用 buffer-list 获得所有缓冲区的列表。

如果你只是想使用一个临时的缓冲区，而不想先建一个缓冲区，使用结束后又需要关闭这个缓冲区，可以用 with-temp-buffer 这个宏。从这个宏的名字可以看出，它所做的事情是先新建一个临时缓冲区，并把这个缓冲区作为当前缓冲区，使用结束后，关闭这个缓冲区，并恢复之前的缓冲区为当前缓冲区。

## 在缓冲区内移动 ##

在学会移动函数之前，先要理解两个概念：位置（position）和标记（mark）。位置是指某个字符在缓冲区内的下标，它从1开始。更准确的说位置是在两个字符之间，所以有在位置之前的字符和在位置之后的字符之说。但是通常我们说在某个位置的字符都是指在这个位置之后的字符。

标记和位置的区别在于位置会随文本插入和删除而改变位置。一个标记包含了缓冲区和位置两个信息。在插入和删除缓冲区里的文本时，所有的标记都会检查一遍，并重新设置位置。这对于含有大量标记的缓冲区处理是很花时间的，所以当你确认某个标记不用的话应该释放这个标记。

创建一个标记使用函数 make-marker。这样产生的标记不会指向任何地方。你需要用 set-marker 命令来设置标记的位置和缓冲区。
{% highlight cl %}
(setq foo (make-marker))             ; => #<marker in no buffer>
(set-marker foo (point))             ; => #<marker at 3594 in *scratch*>
{% endhighlight %}

也可以用 point-marker 直接得到 point 处的标记。或者用 copy-marker 复制一个标记或者直接用位置生成一个标记。
{% highlight cl %}
(point-marker)                       ; => #<marker at 3516 in *scratch*>
(copy-marker 20)                     ; => #<marker at 20 in *scratch*>
(copy-marker foo)                    ; => #<marker at 3502 in *scratch*>
{% endhighlight %}

如果要得一个标记的内容，可以用 marker-position，marker-buffer。
{% highlight cl %}
(marker-position foo)                ; => 3502
(marker-buffer foo)                  ; => #<buffer *scratch*>
{% endhighlight %}

位置就是一个整数，而标记在一般情况下都是以整数的形式使用，所以很多接受整数运算的函数也可以接受标记为参数。比如加减乘。

和缓冲区相关的变量，有的可以用变量得到，比如缓冲区关联的文件名，有的只能用函数来得到，比如 point。point 是一个特殊的缓冲区位置，许多命令在这个位置进行文本插入。每个缓冲区都有一个 point 值，它总是比函数point-min 大，比另一个函数 point-max 返回值小。注意，point-min 的返回值不一定是 1，point-max 的返回值也不定是比缓冲区大小函数 buffer-size 的返回值大 1 的数，因为 emacs 可以把一个缓冲区缩小（narrow）到一个区域，这时 point-min 和 point-max 返回值就是这个区域的起点和终点位置。所以要得到 point 的范围，只能用这两个函数，而不能用 1 和 buffer-size 函数。

和 point 类似，有一个特殊的标记称为 "the mark"。它指定了一个区域的文本用于某些命令，比如 kill-region，indent-region。可以用 mark 函数返回当前 mark 的值。如果使用 transient-mark-mode，而且 mark-even-if-inactive值是 nil 的话，在 mark 没有激活时（也就是 mark-active 的值为 nil），调用 mark 函数会产生一个错误。如果传递一个参数 force 才能返回当前缓冲区 mark 的位置。mark-marker 能返回当前缓冲区的 mark，这不是 mark 的拷贝，所以设置它的值会改变当前 mark 的值。set-mark 可以设置 mark 的值，并激活 mark。每个缓冲区还维护一个mark-ring，这个列表里保存了 mark 的前一个值。当一个命令修改了 mark 的值时，通常要把旧的值放到 mark-ring 里。可以用 push-mark 和 pop-mark 加入或删除 mark-ring 里的元素。当缓冲区里 mark 存在且指向某个位置时，可以用 region-beginning 和 region-end 得到 point 和 mark 中较小的和较大的值。当然如果使用 transient-mark-mode 时，需要激活 mark，否则会产生一个错误。

<dl>
<dt><a href="#answer-mark">思考题</a></dt>
<dd>
 写一个命令，对于使用 transient-mark-mode 时，当选中一个区域时显示区域 的起点和终点，否则显示 point-min 和 point-max 的位置。如果不使用 transient-mark-mode，则显示 point 和 mark 的位置。
 </dd>
</dl>

按单个字符位置来移动的函数主要使用 goto-char 和 forward-char、backward-char。前者是按缓冲区的绝对位置移动，而后者是按 point 的偏移位置移动比如：
{% highlight cl %}
(goto-char (point-min))                   ; 跳到缓冲区开始位置
(forward-char 10)                         ; 向前移动 10 个字符
(forward-char -10)                        ; 向后移动 10 个字符
{% endhighlight %}

可能有一些写 elisp 的人没有读文档或者贪图省事，就在写的 elisp 里直接用 beginning-of-buffer 和 end-of-buffer 来跳到缓冲区的开头和末尾，这其实是不对的。因为这两个命令还做了其它事情，比如设置标记等等。同样，还有一些函数都是不推荐在 elisp 中使用的，如果你准备写一个要发布 elisp，还是要注意一下。

按词移动使用 forward-word 和 backward-word。至于什么是词，这就要看语法表格的定义了。

按行移动使用 forward-line。没有 backward-line。forward-line 每次移动都是移动到行首的。所以，如果要移动到当前行的行首，使用 (forward-line 0)。如果不想移动就得到行首和行尾的位置，可以用 line-beginning-position 和 line-end-position。得到当前行的行号可以用 line-number-at-pos。需要注意的是这个行号是从当前状态下的行号，如果使用 narrow-to-region 或者用 widen 之后都有可能改变行号。

由于 point 只能在 point-min 和 point-max 之间，所以 point 位置测试有时是很重要的，特别是在循环条件测试里。常用的测试函数是 bobp（beginning of buffer predicate）和 eobp（end of buffer predicate）。对于行位置测试使用 bolp（beginning of line predicate）和 eolp（end of line predicate）。

## 缓冲区的内容 ##
要得到整个缓冲区的文本，可以用 buffer-string 函数。如果只要一个区间的文本，使用 buffer-substring 函数。point 附近的字符可以用 char-after 和 char-before 得到。point 处的词可以用 current-word 得到，其它类型的文本，比如符号，数字，S 表达式等等，可以用 thing-at-point 函数得到。

<dl>
<dt><a href="#answer-marksexp">思考题</a></dt>
<dd>
参考 thing-at-point 写一个命令标记光标处的 S 表达式。这个命令和 mark-sexp 不同的是，它能从整个 S 表达式的开始标记。
</dd>
</dl>

## 修改缓冲区的内容 ##

要修改缓冲区的内容，最常见的就是插入、删除、查找、替换了。下面就分别介绍这几种操作。

插入文本最常用的命令是 insert。它可以插入一个或者多个字符串到当前缓冲区的 point 后。也可以用 insert-char 插入单个字符。插入另一个缓冲区的一个区域使用 insert-buffer-substring。

删除一个或多个字符使用 delete-char 或 delete-backward-char。删除一个区间使用 delete-region。如果既要删除一个区间又要得到这部分的内容使用 delete-and-extract-region，它返回包含被删除部分的字符串。

最常用的查找函数是 re-search-forward 和 re-search-backward。这两个函数参数如下：
{% highlight cl %}
(re-search-forward REGEXP &optional BOUND NOERROR COUNT)
(re-search-backward REGEXP &optional BOUND NOERROR COUNT)
{% endhighlight %}

其中 BOUND 指定查找的范围，默认是 point-max（对于 re-search-forward）或 point-min（对于 re-search-backward），NOERROR 是当查找失败后是否要产生一个错误，一般来说在 elisp 里都是自己进行错误处理，所以这个一般设置为 t，这样在查找成功后返回区配的位置，失败后会返回 nil。COUNT 是指定查找匹配的次数。

替换一般都是在查找之后进行，也是使用 replace-match 函数。和字符串的替换不同的是不需要指定替换的对象了。

<dl>
<dt><a href="#answer-replace>思考题</a></dt>
<dd>
 从 OpenOffice 字处理程序里拷贝到 emacs 里的表格通常都是每一个单元格就是一行的。写一个命令，让用户输入表格的列数，把选中区域转换成用制表符分隔的表格。
</dd>
</dl>

## 函数列表 ##
{% highlight cl %}
(buffer-name &optional BUFFER)
(rename-buffer NEWNAME &optional UNIQUE)
(generate-new-buffer-name NAME &optional IGNORE)
(current-buffer)
(set-buffer BUFFER-OR-NAME))
(save-current-buffer &rest BODY)
(with-current-buffer BUFFER-OR-NAME &rest BODY)
(save-excursion &rest BODY)
(get-buffer-create NAME)
(generate-new-buffer NAME)
(kill-buffer BUFFER-OR-NAME)
(buffer-live-p OBJECT)
(buffer-list &optional FRAME)
(with-temp-buffer &rest BODY)
(make-marker)
(set-marker MARKER POSITION &optional BUFFER)
(point-marker)
(copy-marker MARKER &optional TYPE)
(marker-position MARKER)
(marker-buffer MARKER)
(point)
(point-min)
(point-max)
(buffer-size &optional BUFFER)
(mark &optional FORCE)
(mark-marker)
(set-mark POS)
(push-mark &optional LOCATION NOMSG ACTIVATE)
(pop-mark)
(region-beginning)
(region-end)
(goto-char POSITION)
(forward-char &optional N)
(backward-char &optional N)
(beginning-of-buffer &optional ARG)
(end-of-buffer &optional ARG)
(forward-word &optional ARG)
(backward-word &optional ARG)
(forward-line &optional N)
(line-beginning-position &optional N)
(line-end-position &optional N)
(line-number-at-pos &optional POS)
(narrow-to-region START END)
(widen)
(bobp)
(eobp)
(bolp)
(eolp)
(buffer-string)
(buffer-substring START END)
(char-after &optional POS)
(char-before &optional POS)
(current-word &optional STRICT REALLY-WORD)
(thing-at-point THING)
(insert &rest ARGS)
(insert-char CHARACTER COUNT &optional INHERIT)
(insert-buffer-substring BUFFER &optional START END)
(delete-char N &optional KILLFLAG)
(delete-backward-char N &optional KILLFLAG)
(delete-region START END)
(delete-and-extract-region START END)
(re-search-forward REGEXP &optional BOUND NOERROR COUNT)
(re-search-backward REGEXP &optional BOUND NOERROR COUNT)
{% endhighlight %}

## 问题解答 ##

<a name="answer-mark"></a>
#### 可选择区域也可不选择区域的命令 ####
{% highlight cl %}
(defun show-region (beg end)
  (interactive
   (if (or (null transient-mark-mode)
           mark-active)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (message "Region start from %d to %d" beg end))
{% endhighlight %}

这是通常那种如果选择区域则对这个区域应用命令，否则对整个缓冲区应用命令的方法。我喜欢用 transient-mark-mode，因为它让这种作用于区域的命令更灵活。当然也有人反对，无所谓了，emacs 本身就是很个性化的东西。

<a name="answer-marksexp"></a>
#### 标记整个S表达式 ####
{% highlight cl %}
(defun mark-whole-sexp ()
  (interactive)
  (let ((bound (bounds-of-thing-at-point 'sexp)))
    (if bound
        (progn
          (goto-char (car bound))
          (set-mark (point))
          (goto-char (cdr bound)))
      (message "No sexp found at point!"))))
{% endhighlight %}

学习过程中应该可以看看其它一些函数是怎样实现的，从这些源代码中常常能学到很多有用的技巧和方法。比如要标记整个 S 表达式，联想到 thing-at-point 能得到整个 S 表达式，那自然能得到整个S表达式的起点和终点了。所以看看 thing-at-point 的实现，一个很简单的函数，一眼就能发现其中最关键的函数是 bounds-of-thing-at-point，它能返回某个语法实体（syntactic entity）的起点和终点。这样这个命令就很容易就能写出来了。从这个命令中还应该注意到的是对于错误应该很好的处理，让用户明白发生什么错了。比如这里，如果当前光标下没有 S 表达式时，bound 变量为 nil，如果不进行判断，会出现错误：

    Wrong type argument: integer-or-marker-p, nil

加上这个判断，用户就明白发生什么事了。

<a name="answer-replace"></a>
#### oowriter 表格转换 ####
实现这个目的有多种方法：

1. 一行一行移动，删除回车，替换成制表符：
{% highlight cl %}
(defun oowrite-table-convert (col beg end)
  (interactive "nColumns of table: \nr")
  (setq col (1- col))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (dotimes (i col)
          (forward-line 1)
          (backward-delete-char 1)
          (insert-char ?\t 1))
        (forward-line 1)))))
{% endhighlight %}

2. 用 subst-char-in-region 函数直接替换：
{% highlight cl %}
(defun oowrite-table-convert (col beg end)
  (interactive "nColumns of table: \nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (subst-char-in-region
         (point) (progn (forward-line col) (1- (point)))
         ?\n ?\t)))))
{% endhighlight %}

3. 用 re-search-forward 和 replace-match 查找替换
{% highlight cl %}
(defun oowrite-table-convert (col beg end)
  (interactive "nColumns of table: \nr")
  (let (start bound)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (setq start (point))
          (forward-line col)
          (setq bound (copy-marker (1- (point))))
          (goto-char start)
          (while (re-search-forward "\n" bound t)
            (replace-match "\t"))
          (goto-char (1+ bound)))))))
{% endhighlight %}

之所以要给出这三种方法，是想借此说明 elisp 编程其实要实现一个目的通常有
很多种方法，选择一种适合的方法。比如这个问题较好的方法是使用第二种方法，
前提是你要知道有 subst-char-in-region 这个函数，这就要求你对 emacs提供
的内置的函数比较熟悉了，没有别的办法，只有自己多读 elisp manual，如果你
真想学习 elisp 的话，读 manual 还是值得的，我每读一遍都会有一些新的发
现。如果你不知道这个函数，只知道常用的函数，那么相比较而言，第一种方法
是比较容易想到，也比较容易实现的。但是事实上第三种方法才是最重要的方法，
因为这个方法是适用范围最广的。试想一下你如果不是替换两个字符，而是字符
串的话，前面两种方法都没有办法使用了，而这个方法只要稍微修改就能用了。

另外，需要特别说明的是这个命令中 bound 使用的是一个标记而不是一个位置，
如果替换的字符串和删除的字符串是相同长度的，当前用什么都可以，否则就要
注意了，因为在替换之后，边界就有可能改变。这也是写查找替换的函数中很容
易出现的一个错误。解决的办法，一是像我这样用一个标记来记录边界位置。另
一种就是用 narrow-to-region 的方法，先把缓冲区缩小到查找替换的区域，结
束后用 widen 展开。当然为了省事，可以直接用 save-restriction。


