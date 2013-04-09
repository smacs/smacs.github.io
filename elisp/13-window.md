---
layout: page
title: 操作对象之二 ── 窗口
---

首先还是要定义一下什么是窗口（window）。窗口是屏幕上用于显示一个缓冲区
的部分。和它要区分开来的一个概念是 frame。frame 是 Emacs 能够使用屏幕的
部分。可以用窗口的观点来看 frame 和窗口，一个 frame 里可以容纳多个（至
少一个）窗口，而 Emacs 可以有多个 frame。（可能需要和通常所说的窗口的概
念要区分开来，一般来说，我们所说的其它程序的窗口更类似于 Emacs 的一个
frame，所以也有人认为这里 window 译为窗格更好一些。但是窗格这个词是一个
生造出来的词，我还是用窗口比较顺一些，大家自己注意就行了。）在任何时候，
都有一个被选中的 frame，而在这个 frame 里有一个被选中的窗口，称为选择的
窗口（selected window）。

## 分割窗口 ##

刚启动时，emacs 都是只有一个 frame 一个窗口。多个窗口都是用分割窗口的函
数生成的。分割窗口的内建函数是split-window。这个函数的参数如下：

``` cl
(split-window &optional window size horizontal)
```

这个函数的功能是把当前或者指定窗口进行分割，默认分割方式是水平分割，可
以将参数中的 horizontal 设置为 non-nil 的值，变成垂直分割。如果不指定
大小，则分割后两个窗口的大小是一样的。分割后的两个窗口里的缓冲区是同
一个缓冲区。使用这个函数后，光标仍然在原窗口，而返回的新窗口对象：

``` cl
(selected-window)                       ; => #<window 136 on *scratch*>
(split-window)                          ; => #<window 138 on *scratch*>
```

需要注意的是，窗口的分割也需要用树的结构来看分割后的窗口，比如这样一个过程：

```
    +---------------+         +---------------+
    |               |         |      |        |
    | win1          |         | win1 | win2   |
    |               |   -->   |      |        |
    |               |         |      |        |
    |               |         |      |        |
    +---------------+         +---------------+
                                     |
                                     v
    +---------------+         +---------------+
    | win1   |      |         |       |       |
    |        | win2 |         | win1  | win2  |
    |--------|      |   <--   |-------|       |
    | 3 | 4  |      |         | win3  |       |
    |   |    |      |         |       |       |
    +---------------+         +---------------+
```

可以看成是这样一种结构：

``` cl
(win1) ->  (win1 win2) -> ((win1 win3) win2) -> ((win1 (win3 win4)) win2)
```

事实上可以用 window-tree 函数得到当前窗口的结构，如果忽略 minibuffer
对应的窗口，得到的应该类似这样的一个结果：

``` cl
(nil (0 0 170 42)
     (t (0 0 85 42)
        #<win 3>
        (nil (0 21 85 42) #<win 8> #<win 10>))
     #<win 6>)
```

window-tree 返回值的第一个元素代表子窗口的分割方式，nil 表示水平分割，
t 表示垂直分割。第二个元素代表整个结构的大小，这四个数字可以看作是左上
和右下两个顶点的坐标。其余元素是子窗口。每个子窗口也是同样的结构。所以
把前面这个列表还原成窗口排列应该是这样：

```
     (0,0) +-------------------+
           |         |         |
           | win 3   |  win6   |
           |         |         |
    (0,21) |---------|         |
           |    |    |         |
           | 8  | 10 |         |
           |    |    |         |
           +-------------------+ (170, 42)
                   (85, 42)
```

由上面的图可以注意到由 window-tree 返回的结果一些窗口的大小不能确定，
比较上面的 win 8 和 win 10 只能知道它们合并起来的大小，不能确定它们分
别的宽度是多少。

## 删除窗口 ##

如果要让一个窗口不显示在屏幕上，要使用 delete-window 函数。如果没有指定
参数，删除的窗口是当前选中的窗口，如果指定了参数，删除的是这个参数对应
的窗口。删除的窗口多出来的空间会自动加到它的邻接的窗口中。如果要删除除
了当前窗口之外的窗口，可以用 delete-other-windows 函数。

当一个窗口不可见之后，这个窗口对象也就消失了

``` cl
(setq foo (selected-window))            ; => #<window 90 on *scratch*>
(delete-window)
(windowp foo)                           ; => t
(window-live-p foo)                     ; => nil
```

## 窗口配置 ##
窗口配置(window configuration) 包含了 frame 中所有窗口的位置信息：窗口
大小，显示的缓冲区，缓冲区中光标的位置和 mark，还有 fringe，滚动条等等。
用 current-window-configuration 得到当前窗口配置，用
set-window-configuration 来还原

``` cl
(setq foo (current-window-configuration))
;; do sth to make some changes on windows
(set-window-configuration foo)
```

## 选择窗口 ##
可以用 selected-window 得到当前光标所在的窗口

``` cl
(selected-window)                       ; => #<window 104 on *scratch*>
```

可以用 select-window 函数使某个窗口变成选中的窗口

``` cl
(progn
  (setq foo (selected-window))
  (message "Original window: %S" foo)
  (other-window 1)
  (message "Current window: %S" (selected-window))
  (select-window foo)
  (message "Back to original window: %S" foo))
```

两个特殊的宏可以保存窗口位置执行语句：save-selected-window 和
with-selected-window。它们的作用是在执行语句结束后选择的窗口仍留在执行
语句之前的窗口。with-selected-window 和 save-selected-window 几乎相同，
只不过 save-selected-window 选择了其它窗口。这两个宏不会保存窗口的位置
信息，如果执行语句结束后，保存的窗口已经消失，则会选择最后一个选择的窗
口。

``` cl
;; 让另一个窗口滚动到缓冲区开始
(save-selected-window
  (select-window (next-window))
  (goto-char (point-min)))
```

当前 frame 里所有的窗口可以用 window-list 函数得到。可以用 next-window
来得到在 window-list 里排在某个 window 之后的窗口。对应的用
previous-window 得到排在某个 window 之前的窗口。

``` cl
(selected-window)                       ; => #<window 245 on *scratch*>
(window-list)
;; => (#<window 245 on *scratch*> #<window 253 on *scratch*> #<window 251 on *info*>)
(next-window)                           ; => #<window 253 on *scratch*>
(next-window (next-window))             ; => #<window 251 on *info*>
(next-window (next-window (next-window))) ; => #<window 245 on *scratch*>
```

walk-windows 可以遍历窗口，相当于 (mapc proc (window-list))。
get-window-with-predicate 用于查找符合某个条件的窗口。

## 窗口大小信息 ##
窗口是一个长方形区域，所以窗口的大小信息包括它的高度和宽度。用来度量窗
口大小的单位都是以字符数来表示，所以窗口高度为 45 指的是这个窗口可以容
纳 45 行字符，宽度为 140 是指窗口一行可以显示 140 个字符。

mode line 和 header line 都包含在窗口的高度里，所以有 window-height 和
window-body-height 两个函数，后者返回把 mode-line 和 header line 排除后
的高度。

``` cl
(window-height)                         ; => 45
(window-body-height)                    ; => 44
```

滚动条和 fringe 不包括在窗口的亮度里，window-width 返回窗口的宽度

``` cl
(window-width)                          ; => 72
```

也可以用 window-edges 返回各个顶点的坐标信息

``` cl
(window-edges)                          ; => (0 0 73 45)
```

window-edges 返回的位置信息包含了滚动条、fringe、mode line、header
line 在内，window-inside-edges 返回的就是窗口的文本区域的位置

``` cl
(window-inside-edges)                   ; => (1 0 73 44)
```

如果需要的话也可以得到用像素表示的窗口位置信息

``` cl
(window-pixel-edges)                    ; => (0 0 511 675)
(window-inside-pixel-edges)             ; => (7 0 511 660)
```

> ### [思考题](#answer-save-winconf)
> current-window-configuration 可以将当前窗口的位置信
> 息保存到一个变量中以便将来恢复窗口。但是这个对象没有读入形式，所以不
> 能保存到文件中。请写一个函数可以把当前窗口的位置信息生成一个列表，然
> 后用一个函数就能从这个列表恢复窗口。提示：这个列表结构用窗口的分割顺
> 序表示。比如用这样一个列表表示对应的窗口：
>
> ``` cl
> ;; +---------------+
> ;; |   |   |       |
> ;; |   |   |       |
> ;; |-------|       |
> ;; |       |       |
> ;; |       |       |
> ;; +---------------+
> ;; =>
> (horizontal 73
>             (vertical 22
>                       (horizontal 36 win win)
>                       win)
>             win)
> ```

## 窗口对应的缓冲区 ##
窗口对应的缓冲区可以用 window-buffer 函数得到：

``` cl
(window-buffer)                         ; => #<buffer *scratch*>
(window-buffer (next-window))           ; => #<buffer *info*>
```

缓冲区对应的窗口也可以用 get-buffer-window 得到。如果有多个窗口显示同一
个缓冲区，那这个函数只能返回其中的一个，由window-list 决定。如果要得到
所有的窗口，可以用 get-buffer-window-list

``` cl
(get-buffer-window (get-buffer "*scratch*"))
;; => #<window 268 on *scratch*>
(get-buffer-window-list (get-buffer "*scratch*"))
;; => (#<window 268 on *scratch*> #<window 270 on *scratch*>)
```

让某个窗口显示某个缓冲区可以用 set-window-buffer 函数。
让选中窗口显示某个缓冲区也可以用 switch-to-buffer，但是一般不要在
elisp 编程中用这个命令，如果需要让某个缓冲区成为当前缓冲区使用
set-buffer 函数，如果要让当前窗口显示某个缓冲区，使用
set-window-buffer 函数。

让一个缓冲区可见可以用 display-buffer。默认的行为是当缓冲区已经显示在某
个窗口中时，如果不是当前选中窗口，则返回那个窗口，如果是当前选中窗口，
且如果传递的 not-this-window 参数为 non-nil 时，会新建一个窗口，显示缓
冲区。如果没有任何窗口显示这个缓冲区，则新建一个窗口显示缓冲区，并返回
这个窗口。display-buffer 是一个比较高级的命令，用户可以通过一些变量来改
变这个命令的行为。比如控制显示的 pop-up-windows，
display-buffer-reuse-frames，pop-up-frames，控制新建窗口高度的
split-height-threshold，even-window-heights，控制显示的 frame 的
special-display-buffer-names，special-display-regexps，
special-display-function，控制是否应该显示在当前选中窗口
same-window-buffer-names，same-window-regexps 等等。如果这些还不能满
足你的要求（事实上我觉得已经太复杂了），你还可以自己写一个函数，将
display-buffer-function 设置成这个函数。

> ## [思考题](#answer-winconf)
> 前一个思考题只能还原窗口，不能还原缓冲区。请修改一下使它能保存缓冲区信息，还原时让对应的窗口显示对应的缓冲区。

## 改变窗口显示区域 ##
每个窗口会保存一个显示缓冲区的起点位置，这个位置对应于窗口左上角光标在
缓冲区里的位置。可以用 window-start 函数得到某个窗口的起点位置。可以通
过 set-window-start 来改变显示起点位置。可以通过
pos-visible-in-window-p 来检测缓冲区中某个位置是否是可见的。
但是直接通过 set-window-start 来控制显示比较容易出现错误，因为
set-window-start 并不会改变 point 所在的位置，在窗口调用 redisplay 函
数之后 point 会跳到相应的位置。如果你确实有这个需要，我建议还是用：
(with-selected-window window (goto-char pos)) 来代替。

## 函数列表 #

``` cl
(windowp OBJECT)
(split-window &optional WINDOW SIZE HORFLAG)
(selected-window)
(window-tree &optional FRAME)
(delete-window &optional WINDOW)
(delete-other-windows &optional WINDOW)
(current-window-configuration &optional FRAME)
(set-window-configuration CONFIGURATION)
(other-window ARG &optional ALL-FRAMES)
(save-selected-window &rest BODY)
(with-selected-window WINDOW &rest BODY)
(window-list &optional FRAME MINIBUF WINDOW)
(next-window &optional WINDOW MINIBUF ALL-FRAMES)
(previous-window &optional WINDOW MINIBUF ALL-FRAMES)
(walk-windows PROC &optional MINIBUF ALL-FRAMES)
(get-window-with-predicate PREDICATE &optional MINIBUF ALL-FRAMES DEFAULT)
(window-height &optional WINDOW)
(window-body-height &optional WINDOW)
(window-width &optional WINDOW)
(window-edges &optional WINDOW)
(window-inside-edges &optional WINDOW)
(window-pixel-edges &optional WINDOW)
(window-inside-pixel-edges &optional WINDOW)
(window-buffer &optional WINDOW)
(get-buffer-window BUFFER-OR-NAME &optional FRAME)
(get-buffer-window-list BUFFER-OR-NAME &optional MINIBUF FRAME)
(set-window-buffer WINDOW BUFFER-OR-NAME &optional KEEP-MARGINS)
(switch-to-buffer BUFFER-OR-NAME &optional NORECORD)
(display-buffer BUFFER-OR-NAME &optional NOT-THIS-WINDOW FRAME)
(window-start &optional WINDOW)
(set-window-start WINDOW POS &optional NOFORCE)
```

## 问题解答 ##

<a name="answer-save-winconf"></a>
#### 保存窗口位置信息 ####
这是我的答案。欢迎提出改进意见

``` cl
(defun my-window-tree-to-list (tree)
  (if (windowp tree)
      'win
    (let ((dir (car tree))
          (children (cddr tree)))
      (list (if dir 'vertical 'horizontal)
            (if dir
                (my-window-height (car children))
              (my-window-width (car children)))
            (my-window-tree-to-list (car children))
            (if (> (length children) 2)
                (my-window-tree-to-list (cons dir (cons nil (cdr children))))
              (my-window-tree-to-list (cadr children)))))))

(defun my-window-width (win)
  (if (windowp win)
      (window-width win)
    (let ((edge (cadr win)))
      (- (nth 2 edge) (car edge)))))

(defun my-window-height (win)
  (if (windowp win)
      (window-height win)
    (let ((edge (cadr win)))
      (- (nth 3 edge) (cadr edge)))))

(defun my-list-to-window-tree (conf)
  (when (listp conf)
    (let (newwin)
      (setq newwin (split-window nil (cadr conf)
                                 (eq (car conf) 'horizontal)))
      (my-list-to-window-tree (nth 2 conf))
      (select-window newwin)
      (my-list-to-window-tree (nth 3 conf)))))

(defun my-restore-window-configuration (winconf)
  (delete-other-windows)
  (my-list-to-window-tree winconf))

(defun my-current-window-configuration ()
  (my-window-tree-to-list (car (window-tree))))

;; test code here
(setq foo (my-current-window-configuration))
;; do sth to change windows
(my-restore-window-configuration foo)
```

<a name="answer-winconf"></a>
#### 改进的保存窗口信息的函数 ####
由于缓冲区对象也是没有读入形式的，所以返回的列表里只能用缓冲区名来代表
缓冲区，只要没有修改过缓冲区的名字，就能正确的还原缓冲区。如果对于访问
文件的缓冲区，使用文件名可能是更好的想法。保存信息只要对
my-window-tree-to-list 函数做很小的修改就能用了。而恢复窗口则要做较大
改动。my-list-to-window-tree 加了一个函数参数，这样这个函数的可定制性
更高一些。

``` cl
(defun my-window-tree-to-list (tree)
  (if (windowp tree)
      (buffer-name (window-buffer tree))
    (let ((dir (car tree))
          (children (cddr tree)))
      (list (if dir 'vertical 'horizontal)
            (if dir
                (my-window-height (car children))
              (my-window-width (car children)))
            (my-window-tree-to-list (car children))
            (if (> (length children) 2)
                (my-window-tree-to-list (cons dir (cons nil (cdr children))))
              (my-window-tree-to-list (cadr children)))))))

(defun my-list-to-window-tree (conf set-winbuf)
  (let ((newwin (split-window nil (cadr conf)
                              (eq (car conf) 'horizontal))))
    (if (eq (car conf) 'horizontal)
        (progn
          (funcall set-winbuf (selected-window) (nth 2 conf))
          (select-window newwin)
          (if (listp (nth 3 conf))
              (my-list-to-window-tree (nth 3 conf))
            (funcall set-winbuf newwin (nth 3 conf))))
      (if (listp (nth 2 conf))
          (my-list-to-window-tree (nth 2 conf))
        (funcall set-winbuf (selected-window) (nth 2 conf)))
      (select-window newwin)
      (funcall set-winbuf newwin (nth 3 conf)))))

(defun my-restore-window-configuration (winconf)
  (let ((buf (current-buffer)))
    (delete-other-windows)
    (my-list-to-window-tree winconf
                            (lambda (win name)
                              (set-window-buffer win (or (get-buffer name)
                                                         buf))))))
```
