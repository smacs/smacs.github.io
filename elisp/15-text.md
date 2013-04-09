---
layout: page
title: 操作对象之四 ── 文本
---

文本的插入删除，查找替换操作已经在缓冲区一节中讲过了。这一节主要介绍文
本属性。

如果使用过其它图形界面的文本组件进行编程，它们对于文本的高亮一般都是采
用给对应文本贴上相应标签的方法。Emacs 的处理方法也是类似的，但是相比之
下，要强大的多。在 Emacs 里，在不同位置上的每个字符都可以有一个属性列表。
这个属性列表和符号的属性列表很相似，都是由一个名字和值构成的对组成。名
字和值都可以是一个 lisp 对象，但是通常名字都是一个符号，这样可以用这个
符号来查找相应的属性值。复制文本通常都会复制相应的字符的文本属性，但是
也可以用相应的函数只复制文本字符串，比如 substring-no-properties、
insert-buffer-substring-no-properties、buffer-substring-no-properties。

产生一个带属性的字符串可以用 propertize 函数

``` cl
(propertize "abc" 'face 'bold)          ; => #("abc" 0 3 (face bold))
```

如果你在一个 text-mode 的缓冲区内用 M-x eval-expression 用 insert 函数
插入前面这个字符串，就会发现插入的文本已经是粗体字了。之所以不能在
`*scratch*` 产生这种效果，是因为通常我们是开启了
font-lock-mode，在 font-lock-mode 里，文本的 face 属性是实时计算出来的。
在插入文本之后，它的 face 属性已经很快地被改变了。你可以在关闭
font-lock-mode 后再测试一次应该是可以看到 `*scratch*`
里也是可以用这种方法插入带 face 属性的文本的。

虽然文本属性的名字可以是任意的，但是一些名字是有特殊含义的。

|属性名               |含义                                                      |
|---------------------|----------------------------------------------------------|
|category             |值必须是一个符号，这个符号的属性将作为这个字符的属性      |
|face                 |控制文本的字体和颜色                                      |
|font-lock-face       |和 face 相似，可以作为 font-lock-mode 中静态文本的 face   |
|mouse-face           |当鼠标停在文本上时的文本 face                             |
|fontified            |记录是否使用 font lock 标记了 face                        |
|display              |改变文本的显示方式，比如高、低、长短、宽窄，或者用图片代替|
|help-echo            |鼠标停在文本上时显示的文字                                |
|keymap               |光标或者鼠标在文本上时使用的按键映射                      |
|local-map            |和 keymap 类似，通常只使用 keymap                         |
|syntax-table         |字符的语法表                                              |
|read-only            |不能修改文本，通过 stickness 来选择可插入的位置           |
|invisible            |不显示在屏幕上                                            |
|intangible           |把文本作为一个整体，光标不能进入                          |
|field                |一个特殊标记，有相应的函数可以操作带这个标记的文本        |
|cursor               |（不知道具体用途）                                        |
|pointer              |修改鼠标停在文本上时的图像                                |
|line-spacing         |新的一行的距离                                            |
|line-height          |本行的高度                                                |
|modification-hooks   |修改这个字符时调用的函数                                  |
|insert-in-front-hooks|与 modification-hooks 相似，在字符前插入调用的函数        |
|insert-behind-hooks  |与 modification-hooks 相似，在字符后插入调用的函数        |
|point-entered        |当光标进入时调用的函数                                    |
|point-left           |当光标离开时调用的函数                                    |
|composition          |将多个字符显示为一个字形                                  |

正是由于 emacs 的文本有如此丰富的属性，使得 emacs 里的文字才变得多彩，
变得人性化。

## 查看文本属性 ##

由于字符串和缓冲区都可以有文本属性，所以下面的函数通常不提供特定参数就是检
查当前缓冲区的文本属性，如果提供文本对象，则是操作对应的文本属性。

查看文本对象在某处的文本属性可以用 get-text-property 函数。

``` cl
(setq foo (concat "abc"
                  (propertize "cde" 'face 'bold))) ; => #("abccde" 3 6 (face bold))
(get-text-property 3 'face foo)                    ; => bold
(save-excursion
  (goto-char (point-min))
  (insert foo))
(get-text-property 4 'face)                        ; => bold
```

get-char-property 和 get-text-property 相似，但是它是先查找 overlay 的
文本属性。overlay 是缓冲区文字在屏幕上的显示方式，它属于某个缓冲区，具
有起点和终点，也具有文本属性，可以修改缓冲区对应区域上文本的显示方式。

get-text-property 是查找某个属性的值，用 text-properties-at 可以得到某
个位置上文本的所有属性。

## 修改文本属性 ##

put-text-property 可以给文本对象添加一个属性。比如

``` cl
(let ((str "abc"))
  (put-text-property 0 3 'face 'bold str)
  str)                                  ; => #("abc" 0 3 (face bold))
```

和 put-text-property 类似，add-text-properties 可以给文本对象添加一系
列的属性。和 add-text-properties 不同，可以用 set-text-properties 直接
设置文本属性列表。你可以用 =(set-text-properties start end nil)= 来除去
某个区间上的文本属性。也可以用 remove-text-properties 和
remove-list-of-text-properties 来除去某个区域的指定文本属性。这两个函
数的属性列表参数只有名字起作用，值是被忽略的。

``` cl
(setq foo (propertize "abcdef" 'face 'bold
                      'pointer 'hand))
;; => #("abcdef" 0 6 (pointer hand face bold))
(set-text-properties 0 2 nil foo)       ; => t
foo   ; => #("abcdef" 2 6 (pointer hand face bold))
(remove-text-properties 2 4 '(face nil) foo) ; => t
foo   ; => #("abcdef" 2 4 (pointer hand) 4 6 (pointer hand face bold))
(remove-list-of-text-properties 4 6 '(face nil pointer nil) foo) ; => t
foo   ; => #("abcdef" 2 4 (pointer hand))
```

## 查找文本属性 ##

文本属性通常都是连成一个区域的，所以查找文本属性的函数是查找属性变化的
位置。这些函数一般都不作移动，只是返回查找到的位置。使用这些函数时最好
使用 LIMIT 参数，这样可以提高效率，因为有时一个属性直到缓冲区末尾也没
有变化，在这些文本中可能就是多余的。

next-property-change 查找从当前位置起任意一个文本属性发生改变的位置。
next-single-property-change 查找指定的一个文本属性改变的位置。
next-char-property-change 把 overlay 的文本属性考虑在内查找属性发生改
变的位置。next-single-property-change 类似的查找指定的一个考虑 overlay
后文本属性改变的位置。这四个函数都对应有 previous- 开头的函数，用于查
找当前位置之前文本属性改变的位置

``` cl
(setq foo (concat "ab"
                  (propertize "cd" 'face 'bold)
                  (propertize "ef" 'pointer 'hand)))
;; => #("abcdef" 2 4 (face bold) 4 6 (pointer hand))
(next-property-change 1 foo)                  ; => 2
(next-single-property-change 1 'pointer foo)  ; => 4
(previous-property-change 6 foo)              ; => 4
(previous-single-property-change 6 'face foo) ; => 4
```

text-property-any 查找区域内第一个指定属性值为给定值的字符位置。
text-property-not-all 和它相反，查找区域内第一个指定属性值不是给定值的
字符位置。

``` cl
(text-property-any 0 6 'face 'bold foo)          ; => 2
(text-property-any 0 6 'face 'underline foo)     ; => nil
(text-property-not-all 2 6 'face 'bold foo)      ; => 4
(text-property-not-all 2 6 'face 'underline foo) ; => 2
```

> ### [思考题](#answer-fontify)
> 写一个命令，可在 text-mode 里用指定模式给选中的文本添加高亮。

## 函数列表 ##

``` cl
(propertize STRING &rest PROPERTIES)
(get-text-property POSITION PROP &optional OBJECT)
(get-char-property POSITION PROP &optional OBJECT)
(text-properties-at POSITION &optional OBJECT)
(put-text-property START END PROPERTY VALUE &optional OBJECT)
(add-text-properties START END PROPERTIES &optional OBJECT)
(set-text-properties START END PROPERTIES &optional OBJECT)
(remove-text-properties START END PROPERTIES &optional OBJECT)
(remove-list-of-text-properties START END LIST-OF-PROPERTIES &optional OBJECT)
(next-property-change POSITION &optional OBJECT LIMIT)
(next-single-property-change POSITION PROP &optional OBJECT LIMIT)
(next-char-property-change POSITION &optional LIMIT)
(next-single-char-property-change POSITION PROP &optional OBJECT LIMIT)
(previous-property-change POSITION &optional OBJECT LIMIT)
(previous-single-property-change POSITION PROP &optional OBJECT LIMIT)
(previous-char-property-change POSITION &optional LIMIT)
(previous-single-char-property-change POSITION PROP &optional OBJECT LIMIT)
(text-property-any START END PROPERTY VALUE &optional OBJECT)
(text-property-not-all START END PROPERTY VALUE &optional OBJECT)
```

## 问题解答 ##

<a name="answer-fontify"></a>
#### 手工高亮代码 ####

``` cl
(defun my-fontify-region (beg end mode)
  (interactive
   (list (region-beginning)
         (region-end)
         (intern
          (completing-read "Which mode to use: "
                           obarray (lambda (s)
                                     (and (fboundp s)
                                          (string-match "-mode$" (symbol-name s))))
                           t))))
  (let ((buf (current-buffer))
        (font-lock-verbose nil)
        (start 1) face face-list)
    (set-text-properties beg end '(face nil))
    (with-temp-buffer
      (goto-char (point-min))
      (insert-buffer-substring buf beg end)
      (funcall mode)
      (font-lock-fontify-buffer)
      (or (get-text-property start 'face)
          (setq start (next-single-property-change start 'face)))
      (while (and start (< start (point-max)))
        (setq end (or (next-single-property-change start 'face)
                      (point-max))
              face (get-text-property start 'face))
        (and face end (setq face-list (cons (list (1- start) (1- end) face) face-list)))
        (setq start end)))
    (when face-list
      (dolist (f (nreverse face-list))
        (put-text-property (+ beg (car f)) (+ beg (cadr f))
                           'face (nth 2 f))))))
```

但是直接从那个临时缓冲区里把整个代码拷贝出来也可以了，但是可能某些情况
下，不好修改当前缓冲区，或者不想把那个模式里其它文本属性拷贝出来，这个
函数还是有用的。当然最主要的用途是演示使用查找和添加文本属性的方法。事
实上这个函数也是我用来高亮 muse 模式里 src 标签内源代码所用的方法。但是
不幸的是 muse 模式里这个函数并不能产生很好的效果，不知道为什么。


