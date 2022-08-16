(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-golang-generator")
  (ql:quickload "cl-cpp-generator2"))

(in-package :cl-cpp-generator2)

(progn
  (defparameter *max-syscalls* 512)
  (defparameter *target-pid* 0)
 (write-source
  (format nil "~a/stage/cl-golang-generator/examples/32_cilium_ebpf/source01/bpf/bpf.c"
	  (user-homedir-pathname))
  `(do0
    "// +build ignore"
    (include "../../source00/headers/common.h")

    (let ((kprobe_map
	  (designated-initializer
	   :type BPF_MAP_TYPE_PERCPU_ARRAY
	   :key_size (sizeof u32)
	   :value_size (sizeof u64)
	   :max_entries 1)))
     (declare (type "struct bpf_map_def SEC(\"maps\")" kprobe_map))

     (space
      (SEC (string "kprobe/sys_execve"))
      (defun kprobe_execve ()
	(declare (values int))
	(let ((key 0)
	      (initval 1)
	      (valp (bpf_map_lookup_elem
		     &kprobe_map
		     &key)))
	  (declare (type u32 key)
		   (type u64 initval)
		   (type u64* valp))
	  (unless valp
	    (bpf_map_update_elem &kprobe_map
				 &key
				 &initval
				 BPF_ANY)
	    (return 0))
	  (__sync_fetch_and_add
	   valp 1)
	  (return 0))
	)))
    (BPF_PERCPU_ARRAY histogram u32 ,*max-syscalls*)
    (space
     (TRACEPOINT_PROBE raw_syscalls sys_enter)
     (progn
       (let ((pid (>> (bpf_get_current_pid_tgid)
		      32)))
	 (declare (type u64 pid))
	 (unless (== pid ,*target-pid*)
	   (return 0))
	 (let ((key (cast u32 args->id))
	       (value 0)
	       (pval (histogram.lookup_or_try_init &key &value)))
	   (declare (type u32 key value)
		    (type u32* pval))
	   (when pval
	     (incf (aref pval 0)))
	   (return 0)))
       )))))

(in-package :cl-golang-generator)

(progn
  (defparameter *path*
    (format nil "~a/stage/cl-golang-generator/examples/32_cilium_ebpf"
	    (user-homedir-pathname)))
  (defparameter *idx* "01")
  (defun lprint-init ()
    `(defun TimeNow ()
       (declare (values string))
       (return
	 (dot time
	      (Now)
	      (Format
	       (string
		"2006-01-02 15:04:05.000"))))))
  (defun lprint (&key (msg "") vars)
    "generate go code to print variables in log output"
    `(fmt.Printf
      (string
       ,(format nil "%v ~a ~{~a=%v~^ ~}\\n"
		msg (loop for v in vars
			  collect
			  (emit-go :code v))))
      (TimeNow)
      ,@vars
      ))
  (defun tprint (&key (msg "") vars)
    "generate go code to print variables in log output with their types"
    `(fmt.Printf
      (string
       ,(format nil "%v ~a ~{~a=%v (%T)~^ ~}\\n"
		msg vars))
      (TimeNow)
      ,@(loop for e in vars
	      appending
	      `(,e ,e))))
  (let ((err-nr 0))
    (defun panic (var-cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(destructuring-bind
	      (&key (var "_") cmd)
	    var-cmd
	  (prog1
	      `(do0
		(assign (ntuple ,var ,err)
			,cmd)
		(unless (== ,err "nil")
		  ,(lprint :msg (substitute #\' #\" (emit-go :code cmd))
			   :vars `(,err))
		  (panic ,err)))
	    (incf err-nr)))))
    (defun panic0 (cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(prog1
	    `(do0
	      (assign ,err
		      ,cmd)
	      (unless (== ,err "nil")
		,(lprint :msg (substitute #\' #\" (emit-go :code cmd))
			 :vars `(,err))
		(panic ,err))
	      )
	  (incf err-nr))))
    (defun properr (cmd)
      (let ((err (format nil "err~2,'0d" err-nr)))
	(prog1
	    `(do0
	      (assign ,err
		      ,cmd)
	      (unless (== ,err "nil")
		(return ,err))
	      )
	  (incf err-nr)))))

  (let ((file-count 0))
    (defun write-go (&key name code folder)
      (prog1
	  (progn
     	    (let ((dir-name (format nil "~a/source~a/~@[~a/~]"
				    *path*
				    *idx*
				    folder)))
	      (format t "ensure dir-name exists ~a~%" dir-name)
	      (ensure-directories-exist
	       dir-name))
	    (unless folder
	      (with-open-file (s (format nil "~a/source~a/go.mod"
					 *path*
					 *idx*
					 )
				 :direction :output
				 :if-exists nil
				 :if-does-not-exist :create)
		(format s "module bpfexample~%")
		(format s "go 1.18~%")))
	    (write-source
	     (format nil "~a/source~a/~@[~a/~]g~2,'0d_~a"
		     *path* *idx*
		     folder file-count name)
	     code))
	(incf file-count))))

  (let ((name "cltimelog")
	(folder "cltimelog"))
    (write-go
     :name name
     :folder folder
     :code
     `(do0
       (package cltimelog)
       (import
	time
					;fmt
	)
       ,(lprint-init)
       )))

  (let ((name "main")
	(folder nil))
    (write-go
     :name name
     :folder folder
     :code
     `(do0
       "//go:build linux"
       "// +build linux"
       (package main)
       (import
	time
	fmt
	("." bpfexample/cltimelog)
	github.com/cilium/ebpf/link
	github.com/cilium/ebpf/rlimit
	)

       "//go:generate go run github.com/cilium/ebpf/cmd/bpf2go -cc clang-14 -cflags \"-O2 -g -Wall -Werror\" bpf bpf/bpf.c"
       (const "mapKey uint32" 0)
       (defun main ()
	 ,(lprint :msg (format nil "~@[~a/~]~a" folder name))
	 ,(lprint :msg "based on https://github.com/evilsocket/ebpf-process-anomaly-detection/blob/main/lib/ebpf.py")
	 
	 ,(panic0 `(rlimit.RemoveMemlock))

	 (do0
	  ,(lprint :msg "load pre-compiled programs and maps into the kernel")
	  (assign objs (curly bpfObjects))
	  ,(panic0 `(loadBpfObjects &objs "nil"))
	  (defer ((lambda ()
		    ,(lprint :msg "close eBPF objs")
		    (objs.Close)))))

	 (do0
	  ,(lprint :msg "open kernel probe at the entry point of the kernel function and attach the pre-compile program")
	  ,(lprint :msg "a per-cpu counter increases every time the kernel function runs")
	  ,(panic `(:var kp
			 :cmd (link.Kprobe fn
					   objs.KprobeExecve
					   "nil")))
	  (defer ((lambda ()
		    ,(lprint :msg "close kernel probe")
		    (kp.Close)))))

	 (do0
	  ,(lprint :msg "read loop reports every second number of times the kernel function was entered")
	  (assign ticker (time.NewTicker (* 1 time.Second)))
	  (defer (ticker.Stop)))

	 (while (range ticker.C)
	   "var all_cpu_value []uint64"
	   ,(panic0 `(objs.KprobeMap.Lookup mapKey
					    &all_cpu_value))
	   #+nil (foreach ((ntuple cpuid
			     cpuvalue)
		     (range all_cpu_value))
		    ,(lprint :msg "calls" :vars `(fn cpuvalue cpuid)))
	   )
	 )))))
