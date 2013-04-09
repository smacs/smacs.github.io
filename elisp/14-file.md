---
layout: page
title: 操作对象之三 ── 文件
---

作为一个编辑器，自然文件是最重要的操作对象之一。这一节要介绍有关文件的一系列命令，比如查找文件，读写文件，文件信息、读取目录、文件名操作等。

## 打开文件的过程 ##
当你打开一个文件时，实际上 emacs 做了很多事情：

 - 把文件名展开成为完整的文件名
 - 判断文件是否存在
 - 判断文件是否可读或者文件大小是否太大
 - 查看文件是否已经打开，是否被锁定
 - 向缓冲区插入文件内容
 - 设置缓冲区的模式

这还只是简单的一个步骤，实际情况比这要复杂的多，许多异常需要考虑。而且
为了所有函数的可扩展性，许多变量、handler 和 hook 加入到文件操作的函数
中，使得每一个环节都可以让用户或者 elisp 开发者可以定制，甚至完全接管
所有的文件操作。

这里需要区分两个概念：文件和缓冲区。它们是两个不同的对象，文件是在计算
机上可持久保存的信息，而缓冲区是 Emacs 中包含文件内容信息的对象，在
emacs 退出后就会消失，只有当保存缓冲区之后缓冲区里的内容才写到文件中去。

## 文件读写 ##

打开一个文件的命令是 find-file。这命令使一个缓冲区访问某个文件，并让这
个缓冲区成为当前缓冲区。在打开文件过程中会调用 find-file-hook。
find-file-noselect 是所有访问文件的核心函数。与 find-file 不同，它只返
回访问文件的缓冲区。这两个函数都有一个特点，如果 emacs 里已经有一个缓冲
区访问这个文件的话，emacs 不会创建另一个缓冲区来访问文件，而只是简单返
回或者转到这个缓冲区。怎样检查有没有缓冲区是否访问某个文件呢？所有和文
件关联的缓冲区里都有一个 buffer-local 变量buffer-file-name。但是不要直
接设置这个变量来改变缓冲区关联的文件。而是使用 set-visited-file-name 来
修改。同样不要直接从 buffer-list 里搜索buffer-file-name 来查找和某个文
件关联的缓冲区。应该使用get-file-buffer 或者 find-buffer-visiting。

{% highlight cl %}
(find-file "~/temp/test.txt")
(with-current-buffer
    (find-file-noselect "~/temp/test.txt")
  buffer-file-name)                     ; => "/home/ywb/temp/test.txt"
(find-buffer-visiting "~/temp/test.txt") ; => #<buffer test.txt>
(get-file-buffer "~/temp/test.txt")      ; => #<buffer test.txt>
{% endhighlight %}


保存一个文件的过程相对简单一些。首先创建备份文件，处理文件的位模式，将
缓冲区写入文件。保存文件的命令是 save-buffer。相当于其它编辑器里另存为
的命令是 write-file。在这个过程中会调用一些函数或者 hook。
write-file-functions 和 write-contents-functions 几乎功能完全相同。它们
都是在写入文件之前运行的函数，如果这些函数中有一个返回了 non-nil 的值，
则会认为文件已经写入了，后面的函数都不会运行，而且也不会使用再调用其它
写入文件的函数。这两个变量有一个重要的区别是write-contents-functions 在
改变主模式之后会被修改，因为它没有permanent-local 属性，而
write-file-functions 则会仍然保留。before-save-hook 和
write-file-functions 功能也比较类似，但是这个变量里的函数会逐个执行，不
论返回什么值也不会影响后面文件的写入。after-save-hook 是在文件已经写入
之后才调用的 hook，它是 save-buffer 最后一个动作。

但是实际上在 elisp 编程过程中经常遇到的一个问题是读取一个文件中的内容，
读取完之后并不希望这个缓冲区还留下来，如果直接用 kill-buffer 可能会把
用户打开的文件关闭。而且 find-file-noselect 做的事情实在超出我们的需要
的。这时你可能需要的是更底层的文件读写函数，它们是
insert-file-contents 和 write-region，调用形式分别是：
{% highlight cl %}
(insert-file-contents filename &optional visit beg end replace)
(write-region start end filename &optional append visit lockname mustbenew)
{% endhighlight %}

insert-file-contents 可以插入文件中指定部分到当前缓冲区中。如果指定
visit 则会标记缓冲区的修改状态并关联缓冲区到文件，一般是不用的。
replace 是指是否要删除缓冲区里其它内容，这比先删除缓冲区其它内容后插入文
件内容要快一些，但是一般也用不上。insert-file-contents 会处理文件的编
码，如果不需要解码文件的话，可以用 insert-file-contents-literally。

write-region 可以把缓冲区中的一部分写入到指定文件中。如果指定 append
则是添加到文件末尾。和 insert-file-contents 相似，visit 参数也会把缓冲
区和文件关联，lockname 则是文件锁定的名字，mustbenew 确保文件存在时会
要求用户确认操作。

> ### [思考题](#answer-header-function)
> 写一个函数提取出某个 c 头文件中的函数声明中的函数名和声明位置。

## 文件信息 ##

文件是否存在可以使用 file-exists-p 来判断。对于目录和一般文件都可以用
这个函数进行判断，但是符号链接只有当目标文件存在时才返回 t。

如何判断文件是否可读或者可写呢？file-readable-p、file-writable-p，
file-executable-p 分用来测试用户对文件的权限。文件的位模式还可以用
file-modes 函数得到。

{% highlight cl %}
(file-exists-p "~/temp/test.txt")              ; => t
(file-readable-p "~/temp/test.txt")            ; => t
(file-writable-p "~/temp/test.txt")            ; => t
(file-executable-p "~/temp/test.txt")          ; => nil
(format "%o" (file-modes "~/temp/test.txt"))   ; => "644"
{% endhighlight %}

文件类型判断可以使用 file-regular-p、file-directory-p、file-symlink-p，
分别判断一个文件名是否是一个普通文件（不是目录，命名管道、终端或者其它
IO 设备）、文件名是否一个存在的目录、文件名是否是一个符号链接。其中
file-symlink-p 当文件名是一个符号链接时会返回目标文件名。文件的真实名
字也就是除去相对链接和符号链接后得到的文件名可以用 file-truename 得到。
事实上每个和文件关联的 buffer 里也有一个缓冲区局部变量
buffer-file-truename 来记录这个文件名。

    $ ls -l t.txt
    lrwxrwxrwx 1 ywb ywb 8 2007-07-15 15:51 t.txt -> test.txt

{% highlight cl %}
(file-regular-p "~/temp/t.txt")         ; => t
(file-directory-p "~/temp/t.txt")       ; => nil
(file-symlink-p "~/temp/t.txt")         ; => "test.txt"
(file-truename "~/temp/t.txt")          ; => "/home/ywb/temp/test.txt"
{% endhighlight %}

文件更详细的信息可以用 file-attributes 函数得到。这个函数类似系统的
stat 命令，返回文件几乎所有的信息，包括文件类型，用户和组用户，访问日
期、修改日期、status change 日期、文件大小、文件位模式、inode number、
system number。这是我写的方便使用的帮助函数：

{% highlight cl %}
(defun file-stat-type (file &optional id-format)
  (car (file-attributes file id-format)))
(defun file-stat-name-number (file &optional id-format)
  (cadr (file-attributes file id-format)))
(defun file-stat-uid (file &optional id-format)
  (nth 2 (file-attributes file id-format)))
(defun file-stat-gid (file &optional id-format)
  (nth 3 (file-attributes file id-format)))
(defun file-stat-atime (file &optional id-format)
  (nth 4 (file-attributes file id-format)))
(defun file-stat-mtime (file &optional id-format)
  (nth 5 (file-attributes file id-format)))
(defun file-stat-ctime (file &optional id-format)
  (nth 6 (file-attributes file id-format)))
(defun file-stat-size (file &optional id-format)
  (nth 7 (file-attributes file id-format)))
(defun file-stat-modes (file &optional id-format)
  (nth 8 (file-attributes file id-format)))
(defun file-stat-guid-changep (file &optional id-format)
  (nth 9 (file-attributes file id-format)))
(defun file-stat-inode-number (file &optional id-format)
  (nth 10 (file-attributes file id-format)))
(defun file-stat-system-number (file &optional id-format)
  (nth 11 (file-attributes file id-format)))
(defun file-attr-type (attr)
  (car attr))
(defun file-attr-name-number (attr)
  (cadr attr))
(defun file-attr-uid (attr)
  (nth 2 attr))
(defun file-attr-gid (attr)
  (nth 3 attr))
(defun file-attr-atime (attr)
  (nth 4 attr))
(defun file-attr-mtime (attr)
  (nth 5 attr))
(defun file-attr-ctime (attr)
  (nth 6 attr))
(defun file-attr-size (attr)
  (nth 7 attr))
(defun file-attr-modes (attr)
  (nth 8 attr))
(defun file-attr-guid-changep (attr)
  (nth 9 attr))
(defun file-attr-inode-number (attr)
  (nth 10 attr))
(defun file-attr-system-number (attr)
  (nth 11 attr))
{% endhighlight %}

前一组函数是直接由文件名访问文件信息，而后一组函数是由 file-attributes
的返回值来得到文件信息。

## 修改文件信息 ##

重命名和复制文件可以用 rename-file 和 copy-file。删除文件使用
delete-file。创建目录使用 make-directory 函数。不能用 delete-file 删除
目录，只能用 delete-directory 删除目录。当目录不为空时会产生一个错误。

设置文件修改时间使用 set-file-times。设置文件位模式可以用
set-file-modes 函数。set-file-modes函数的参数必须是一个整数。你可以用位
函数 logand、logior 和 logxor 函数来进行位操作。

> ### [思考题](#answer-chmod)
> 写一个函数模拟 chmod 命令的行为。

## 文件名操作 ##

虽然 MSWin 的文件名使用的路径分隔符不同，但是这里介绍的函数都能用于
MSWin 形式的文件名，只是返回的文件名都是 Unix 形式了。路径一般由目录和
文件名，而文件名一般由主文件名(basename)、文件名后缀和版本号构成。
Emacs 有一系列函数来得到路径中的不同部分：
{% highlight cl %}
(file-name-directory "~/temp/test.txt")      ; => "~/temp/"
(file-name-nondirectory "~/temp/test.txt")   ; => "test.txt"
(file-name-sans-extension "~/temp/test.txt") ; => "~/temp/test"
(file-name-extension "~/temp/test.txt")      ; => "txt"
(file-name-sans-versions "~/temp/test.txt~") ; => "~/temp/test.txt"
(file-name-sans-versions "~/temp/test.txt.~1~") ; => "~/temp/test.txt"
{% endhighlight %}

路径如果是从根目录开始的称为是绝对路径。测试一个路径是否是绝对路径使用
file-name-absolute-p。如果在 Unix 或 GNU/Linux 系统，以
`~` 开头的路径也是绝对路径。在 MSWin 上，以 "/" 、
"\\"、"X:" 开头的路径都是绝对路径。如果不是绝对路径，可以使用
expand-file-name 来得到绝对路径。把一个绝对路径转换成相对某个路径的相
对路径的可以用 file-relative-name 函数。

{% highlight cl %}
(file-name-absolute-p "~rms/foo")       ; => t
(file-name-absolute-p "/user/rms/foo")  ; => t
(expand-file-name "foo")                ; => "/home/ywb/foo"
(expand-file-name "foo" "/usr/spool/")  ; => "/usr/spool/foo"
(file-relative-name "/foo/bar" "/foo/") ; => "bar"
(file-relative-name "/foo/bar" "/hack/") ; => "../foo/bar"
{% endhighlight %}

对于目录，如果要将其作为目录，也就是确保它是以路径分隔符结束，可以用
file-name-as-directory。不要用 (concat dir "/") 来转换，这会有移植问题。
和它相对应的函数是 directory-file-name。
{% highlight cl %}
(file-name-as-directory "~rms/lewis")   ; => "~rms/lewis/"
(directory-file-name "~lewis/")         ; => "~lewis"
{% endhighlight %}

如果要得到所在系统使用的文件名，可以用 convert-standard-filename。比如
在 MSWin 系统上，可以用这个函数返回用 "\\" 分隔的文件名。
{% highlight cl %}
(convert-standard-filename "c:/windows")  ;=> "c:\\windows"
{% endhighlight %}

## 临时文件 ##

如果需要产生一个临时文件，可以使用 make-temp-file。这个函数按给定前缀产
生一个不和现有文件冲突的文件，并返回它的文件名。如果给定的名字是一个相
对文件名，则产生的文件名会用 temporary-file-directory 进行扩展。也可以
用这个函数产生一个临时文件夹。如果只想产生一个不存在的文件名，可以用
make-temp-name 函数。
{% highlight cl %}
(make-temp-file "foo")                  ; => "/tmp/foo5611dxf"
(make-temp-name "foo")                  ; => "foo5611q7l"
{% endhighlight %}

## 读取目录内容 ##

可以用 directory-files 来得到某个目录中的全部或者符合某个正则表达式的
文件名。

{% highlight cl %}
(directory-files "~/temp/dir/")
;; =>
;; ("#foo.el#" "." ".#foo.el" ".." "foo.el" "t.pl" "t2.pl")
(directory-files "~/temp/dir/" t)
;; =>
;; ("/home/ywb/temp/dir/#foo.el#"
;;  "/home/ywb/temp/dir/."
;;  "/home/ywb/temp/dir/.#foo.el"
;;  "/home/ywb/temp/dir/.."
;;  "/home/ywb/temp/dir/foo.el"
;;  "/home/ywb/temp/dir/t.pl"
;;  "/home/ywb/temp/dir/t2.pl")
(directory-files "~/temp/dir/" nil "\\.pl$") ; => ("t.pl" "t2.pl")
{% endhighlight %}

directory-files-and-attributes 和 directory-files 相似，但是返回的列表
中包含了 file-attributes 得到的信息。file-name-all-versions 用于得到某
个文件在目录中的所有版本，file-expand-wildcards 可以用通配符来得到目录
中的文件列表。

> ### [思考题](#answer-recursive-ls)
> 写一个函数返回当前目录包括子目录中所有文件名。

## 神奇的 Handle ##

如果不把文件局限在存储在本地机器上的信息，如果有一套基本的文件操作，比
如判断文件是否存在、打开文件、保存文件、得到目录内容之类，那远程的文件
和本地文件的差别也仅在于文件名表示方法不同而已。在 Emacs 里，底层的文件
操作函数都可以托管给 elisp 中的函数，这样只要用 elisp 实现了某种类型文
件的基本操作，就能像编辑本地文件一样编辑其它类型文件了。

决定何种类型的文件名使用什么方式来操作是在 file-name-handler-alist 变
量定义的。它是由形如 `(REGEXP . HANDLER)` 的列表。如果文件名匹配这个
REGEXP 则使用 HANDLER 来进行相应的文件操作。这里所说的文件操作，具体的
来说有这些函数：

    `access-file', `add-name-to-file', `byte-compiler-base-file-name',
    `copy-file', `delete-directory', `delete-file',
    `diff-latest-backup-file', `directory-file-name', `directory-files',
    `directory-files-and-attributes', `dired-call-process',
    `dired-compress-file', `dired-uncache',
    `expand-file-name', `file-accessible-directory-p', `file-attributes',
    `file-directory-p', `file-executable-p', `file-exists-p',
    `file-local-copy', `file-remote-p', `file-modes',
    `file-name-all-completions', `file-name-as-directory',
    `file-name-completion', `file-name-directory', `file-name-nondirectory',
    `file-name-sans-versions', `file-newer-than-file-p',
    `file-ownership-preserved-p', `file-readable-p', `file-regular-p',
    `file-symlink-p', `file-truename', `file-writable-p',
    `find-backup-file-name', `find-file-noselect',
    `get-file-buffer', `insert-directory', `insert-file-contents',
    `load', `make-auto-save-file-name', `make-directory',
    `make-directory-internal', `make-symbolic-link',
    `rename-file', `set-file-modes', `set-file-times',
    `set-visited-file-modtime', `shell-command', `substitute-in-file-name',
    `unhandled-file-name-directory', `vc-registered',
    `verify-visited-file-modtime',
    `write-region'.

在 HANDLE 里，可以只接管部分的文件操作，其它仍交给 emacs 原来的函数来完
成。举一个简单的例子。比如最新版本的 emacs 把
`*scratch*` 的 auto-save-mode 打开了。如果你不想这个缓
冲区的自动保存的文件名散布得到处都是，可以想办法让这个缓冲区的自动保存
文件放到指定的目录中。刚好 make-auto-save-file-name 是在上面这个列表里
的，但是不幸的是在函数定义里 make-auto-save-file-name 里不对不关联文件
的缓冲区使用 handler（我觉得是一个 bug 呀），继续往下看，发现生成保存文
件名是使用了 expand-file-name 函数，所以解决办法就产生了：

{% highlight cl %}
(defun my-scratch-auto-save-file-name (operation &rest args)
  (if (and (eq operation 'expand-file-name)
           (string= (car args) "#*scratch*#"))
      (expand-file-name (concat "~/.emacs.d/backup/" (car args)))
    (let ((inhibit-file-name-handlers
           (cons 'my-scratch-auto-save-file-name
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args))))
{% endhighlight %}

## 函数列表 ##

{% highlight cl %}
(find-file FILENAME &optional WILDCARDS)
(find-file-noselect FILENAME &optional NOWARN RAWFILE WILDCARDS)
(set-visited-file-name FILENAME &optional NO-QUERY ALONG-WITH-FILE)
(get-file-buffer FILENAME)
(find-buffer-visiting FILENAME &optional PREDICATE)
(save-buffer &optional ARGS)
(insert-file-contents FILENAME &optional VISIT BEG END REPLACE)
(insert-file-contents-literally FILENAME &optional VISIT BEG END REPLACE)
(write-region START END FILENAME &optional APPEND VISIT LOCKNAME MUSTBENEW)
(file-exists-p FILENAME)
(file-readable-p FILENAME)
(file-writable-p FILENAME)
(file-executable-p FILENAME)
(file-modes FILENAME)
(file-regular-p FILENAME)
(file-directory-p FILENAME)
(file-symlink-p FILENAME)
(file-truename FILENAME)
(file-attributes FILENAME &optional ID-FORMAT)
(rename-file FILE NEWNAME &optional OK-IF-ALREADY-EXISTS)
(copy-file FILE NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)
(delete-file FILENAME)
(make-directory DIR &optional PARENTS)
(delete-directory DIRECTORY)
(set-file-modes FILENAME MODE)
(file-name-directory FILENAME)
(file-name-nondirectory FILENAME)
(file-name-sans-extension FILENAME)
(file-name-sans-versions NAME &optional KEEP-BACKUP-VERSION)
(file-name-absolute-p FILENAME)
(expand-file-name NAME &optional DEFAULT-DIRECTORY)
(file-relative-name FILENAME &optional DIRECTORY)
(file-name-as-directory FILE)
(directory-file-name DIRECTORY)
(convert-standard-filename FILENAME)
(make-temp-file PREFIX &optional DIR-FLAG SUFFIX)
(make-temp-name PREFIX)
(directory-files DIRECTORY &optional FULL MATCH NOSORT)
(dired-files-attributes DIR)
{% endhighlight %}

## 问题解答 ##

<a name="answer-header-function"></a>
#### 提取头文件中函数名 ####
这是我写的一个版本，主要是函数声明的正则表达式不好写，函数是很简单的。
从这个例子也可以看出它错误的把那个 typedef void 当成函数声明了。如果你
知道更好的正则表达式，请告诉我一下。

{% highlight cl %}
(defvar header-regexp-list
  '(("^\\(?:\\(?:G_CONST_RETURN\\|extern\\|const\\)\\s-+\\)?[a-zA-Z][_a-zA-Z0-9]*\
\\(?:\\s-*[*]*[ \t\n]+\\|\\s-+[*]*\\)\\([a-zA-Z][_a-zA-Z0-9]*\\)\\s-*(" . 1)
    ("^\\s-*#\\s-*define\\s-+\\([a-zA-Z][_a-zA-Z0-9]*\\)" . 1)))
(defun parse-c-header (file)
  "Extract function name and declaration position using
`header-regexp-list'."
  (interactive "fHeader file: \nP")
  (let (info)
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (re header-regexp-list)
        (goto-char (point-min))
        (while (re-search-forward (car re) nil t)
          (push (cons (match-string (cdr re)) (line-beginning-position)) info))))
    info))
(parse-c-header "/usr/include/glib-2.0/gmodule.h")
;; =>
;; (("g_module_name" . 1788)
;;  ("g_module_open" . 1747)
;;  ("G_MODULE_EXPORT" . 1396)
;;  ("G_MODULE_EXPORT" . 1317)
;;  ("G_MODULE_IMPORT" . 1261)
;;  ("g_module_build_path" . 3462)
;;  ("g_module_name" . 2764)
;;  ("g_module_symbol" . 2570)
;;  ("g_module_error" . 2445)
;;  ("g_module_make_resident" . 2329)
;;  ("g_module_close" . 2190)
;;  ("g_module_open" . 2021)
;;  ("g_module_supported" . 1894)
;;  ("void" . 1673))
{% endhighlight %}

<a name="answer-chmod"></a>
#### 模拟 chmod 的函数 ####

这是一个改变单个文件模式的 chmod 版本。递归版本的就自己作一个练习吧。最
好不要直接调用这个函数，因为每次调用都要解析一次 mode 参数，想一个只解
析一次的方法吧。

{% highlight cl %}
(defun chmod (mode file)
  "A elisp function to simulate command chmod.
Note that the command chmod can accept MODE match
`[ugoa]*([-+=]([rwxXst]*|[ugo]))+', but this version only can process
MODE match `[ugoa]*[-+=]([rwx]*|[ugo])'.
"
  (cond ((integerp mode)
         (if (> mode #o777)
             (error "Unknown mode option: %d" mode)))
        ((string-match "^[0-7]\\{3\\}$" mode)
         (setq mode (string-to-number mode 8)))
        ((string-match "^\\([ugoa]*\\)\\([-+=]\\)\\([rwx]*\\|[ugo]\\)$" mode)
         (let ((users (append (match-string 1 mode) nil))
               (mask-func (string-to-char (match-string 2 mode)))
               (bits (append (match-string 3 mode) nil))
               (oldmode (file-modes file))
               (user-list '((?a . #o777)
                            (?u . #o700)
                            (?g . #o070)
                            (?o . #o007)))
               mask)
           (when bits
             (setq bits (* (cond ((= (car bits) ?u)
                                  (lsh (logand oldmode #o700) -6))
                                 ((= (car bits) ?g)
                                  (lsh (logand oldmode #o070) -3))
                                 ((= (car bits) ?o)
                                  (logand oldmode #o007))
                                 (t
                                  (+ (if (member ?r bits) 4 0)
                                     (if (member ?w bits) 2 0)
                                     (if (member ?x bits) 1 0))))
                           #o111))
             (if users
                 (setq mask (apply 'logior
                                   (delq nil (mapcar
                                              (lambda (u)
                                                (assoc-default u user-list))
                                              users))))
               (setq mask #o777))
             (setq mode
                   (cond ((= mask-func ?\=)
                          (logior (logand bits mask)
                                  (logand oldmode (logxor mask #o777))))
                         ((= mask-func ?\+)
                          (logior oldmode (logand bits mask)))
                         (t
                          (logand oldmode
                                  (logxor (logand bits mask) #o777))))))))
        (t (error "Unknow mode option: %S" mode)))
  (set-file-modes file mode))
{% endhighlight %}

<a name="answer-recursive-ls"></a>
#### 列出目录中所有文件 ####
为了让这个函数更类似 directory-files 函数，我把参数设置为和它一样的：

{% highlight cl %}
(defun my-directory-all-files (dir &optional full match nosort)
  (apply 'append
   (delq nil
    (mapcar
     (lambda (file)
       (if (and (not (string-match "^[.]+$" (file-name-nondirectory file)))
                (file-directory-p (expand-file-name file dir)))
           (if full
               (my-directory-all-files file full match nosort)
             (mapcar (lambda (f)
                       (concat (file-name-as-directory file) f))
                     (my-directory-all-files (expand-file-name file dir)
                                             full match nosort)))
         (if (string-match match file)
             (list file))))
     (directory-files dir full nil nosort)))))
{% endhighlight %}


