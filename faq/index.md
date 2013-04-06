---
layout: page
title: 水木社区Emacs版FAQ
---

## General

### 怎样才能语法加亮

将以下写到.emacs文件: `(global-font-lock-mode t)`

### 在Windows版的Emacs下怎样使用ispell?

Win32有一个native的ispell v4，但很不好用，建议装一个cygwin的ispell-3.2.06

### Windows版的Emacs为什么不能显示图片?

使用cvs版的ntemacs,在.emacs中加入: `(auto-image-file-mode t)`

### 设置打开文件的缺省路径

`(setq default-directory "~")`

### ".emacs"在哪里？

- Unix/Linux
    ~/.emacs 如果没有，自己创建一个。 `touch .emacs`.
- Windows
    在根目录下创建一个名为".emacs"的文件, 如果创建不了的话也可以用"_emacs"作为文件名. 主目录由windows的环境变量HOME指定，如果没有设定HOME环境变量，一般为C:\

### 下载的el文件怎么用？

把文件放到load-path中去，然后按照文件头上的注释说明做。 ~/site-lisp一般默认在load-path中.
增加load-path例: `(add-to-list 'load-path "/path/to/yours")`


### 怎么进行矩形区域操作？(类似UltraEdit的列模式) 怎么在每一行前插入相同文字？

x-10-3-1

### 总有个带小尾巴~的文件出现(自动备份文件)，怎么办？

小尾巴文件是备份，可以干掉它或者按文件名分类放到指定目录中去。详细见x-8-8

### 我想把一个命令绑定到某个按键组合上，怎么表示组合键？

x-8-6

### 怎么转换文件格式？(DOS <--> UNIX)

C-x <RET> f  (函数 `set-buffer-file-coding-system` )

### 查找和替换的时候怎么输入回车之类的特殊字符？

C-q之后输入该特殊字符
例如: ^M是C-q C-m，回车是C-q C-j，TAB是C-q TAB

### M-x shell进入shell-mode出现乱码？怎么修改颜色和字体？

在.emacs中加入: `(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)`
更多选项请:M-x customize-group ansi-colors

### windows下插入、浏览图片时emacs崩溃

windows版本的emacs的图片支持dll是用mingw编译的，因此如果你的emacs是用VC编译
的，则可能出现意想不到的问题。建议选择用[mingw编译的emacs](http://ntmacs.sf.net)

### 如何跳到某一行?

"M-x goto-line", 推荐绑定在 "M-g" 键上, Emacs22默认把它绑定在 M-g g 上。

### 如何插入当前时间?

一个例子:

``` scheme
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))

(global-set-key (kbd "C-c m d") 'my-insert-date)
```

### 修改 .emacs 后即时生效(不重启)?

有几种方法:

1. M-x eval-last-sexp(or C-x C-e)
2. M-x eval-region
3. M-x eval-buffer

### 如何redo？

undo(绑定到 C-/)的过程中，进行一下其它动作，比如 C-f 移动一下光标，随后继续undo(C-/)就是redo。
或者使用redo.el

### 如何自动换行?

设置 `(toggle-truncate-lines t)`

### 为何M-w无效？

如果是windows系统，很可能是QQ热键拦截

## Chinese Related

### Emacs与其他程序间为什么不能拷贝、粘贴

在~/.emacs加入这句话 `(setq x-select-enable-clipboard t)`

### Emacs中新开buffer不使用默认字体怎么办
``` scheme
(add-to-list 'default-frame-alist
 '(font . "-*-SimSun-medium-r-normal-*-12-*"))
```

### XEmacs怎样保存中文
``` scheme
(prefer-coding-system 'gb2312)
```

### 怎样使Emacs支持GBK和GB18030

Emacs21/22不支持GBK和GB18030。

如果你用的是Emacs21/22, 请[下载mule-gbk](http://mule-gbk.sourceforge.net)，按照里面的README进行安装。这样可以使Emacs21/22支持GBK。

无法使Emacs21/22支持GB18030。

Emacs23/24支持GBK和GB18030.

### 怎样在文件中指定编码

Emacs具有自动识别文件编码的功能，不过很多编码方案都是类似的，没有什么算法能区分这些编码方案，因此，Emacs的自动识别不是万能的，有时候Emacs会误判文件的编码。

只有文件的作者最清楚文件采用的是什么编码，因此我们提倡在文件中指定编码。在文件中指定编码的方法有两种:

1. 在文件的第一行(对于脚本文件可以是第二行)加上:

    ``` bash
    -*- coding: gb2312; -*-
    ```

    对于源程序文件可以在前面加注释符号，如

    ``` cpp
    /*     -*- coding: gb2312; -*-     */       /* C语言程序 */
    //      -*- coding: gb2312; -*-             // C++语言程序
    ```

2. 在文件的末尾使用文件变量(Local Variables), 例如在 TeX 文件末尾，可以加上:

    ``` tex
    %% Local Variables:           % 是 Tex 文件的注释符号，
    %% coding: gbk                  可以根据情况换成其它注释符号，
    %% End:                         普通文本也可以不加注释符号。
    ```

## TeX

### Windows下不能使用LaTeX-preview

使用CVS的GNU Emacs或XEmacs就可以了

### Emacs能象vimsuite那样加载类似CJK的模板么?

可以，使用template.el扩展可以加载任何格式的模板

## Programming

### 怎样把C/C++文件重新排版

选择文本块,ESC C-\,M-X c-set-style,可以设置不同的缩进风格

### c/c++, java 自动补全功能?

c/c++ - ecb
java  - jde

Both kind of *SLOW*, though.

### 如何在Emacs中编写调试c/c++程序

M-x c-mode/ M-x cc-mode

M-x compile

M-x gdb

## Gnus, Mail, Web

### mew怎样使用SMTP验证?

``` scheme
(setq mew-smtp-auth-list (quote ("CRAM-MD5" "LOGIN" "PLAIN")))
```
### 怎样使mew在pop取信时将信件保留在服务器上?

``` scheme
(setq mew-pop-delete nil)
```

### gnus怎样使用通讯录?

可以用 bbdb

## Download

### 获取最新版本的GNU Emacs源码

最新源码可以使用cvs方式:

``` bash
cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/emacs co emacs
```

windows用户可以用cygnus下的cvs(推荐), 也可以装一个WinCVS

``` bash
bzr branch --stacked http://bzr.savannah.gnu.org/r/emacs/trunk
```

- 从 官方 bzr 库镜像而来

    ``` bash
    git clone git://repo.or.cz/emacs.git
    git clone http://repo.or.cz/r/emacs.git
    ```

- 从 repo.or.cz 的 git 库镜像而来

    ``` bash
    git clone git://github.com/emacsmirror/emacs.git
    git clone http://github.com/emacsmirror/emacs.git

    git clone git://gitorious.org/emacsmirror/emacs.git
    git clone http://git.gitorious.org/emacsmirror/emacs.git
    ```

可以给 git clone 加 --depth 1 参数以加快速度，见 git help clone。

### Windows用户如何获取GNU Emacs编译版本

- [一个更新较快的站点](http://ourcomments.org/cgi-bin/emacsw32-dl-latest.pl)
- [下载NTemacs的最新CVS版](http://ntemacs.sourceforge.net/)

### Mac用户如何获取GNU Emacs编译版本

- [AquaEmacs](http://aquamacs.org/)
- [GNU Emacs For Mac OS X](http://emacsformacosx.com/)
- [Emacs Mac Port](https://github.com/railwaycat/emacs-mac-port)

## Resources

### 推荐Emacs入门书籍

- Sams Teach Yourself Emacs in 24 Hours
- 《学习GNU Emacs》

### 推荐Emacs Lisp入门书籍

- GNU Emacs Lisp编程入门
    涵盖了入门需要的几乎所有知识，写给没用过Emacs，没学过lisp；
    但是用过编辑器，会写程序的人看的入门书。这本书最好用info看，
    一边看一边就可以试。在info里面显示为Emacs Lisp Intro。
- GNU Emacs Manual
    速查手册，info里面显示为Elisp。
- 《Writing GNU Emacs Extensions》by Bob Glickstein, O'REILLY

### 一个收集大量Emacs配置文件的网站

http://www.dotemacs.de/
