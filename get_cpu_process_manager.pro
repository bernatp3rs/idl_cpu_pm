;+
; @name 
;   cpu_process_manager
; 
; @author 
;   Bernat Puigdomenech Treserras
;   e-mail: bernatp3rs@gmail.com
; 
; @purpose 
;   The CPU_Process_Manager library allows you to manage parallel computing under IDL. 
;
;   The basic idea of this library is to allow the control over a set of IDL sessions in a flexible way. 
;   From the main session, the user can create the cpu_process_manager that will initialize a set of xidl_idlbridges 
;   that will control IDL slave sessions. Every session can run as a separate process and can exchange data with the
;   main session by copying IDL variables. If your variables are defined as shared memory segments, all sessions
;   will be able to "see" them at the same time without copying (duplicating) anything. 
;   The execution of every session is independent of the other sessions and they will all run asynchronously, allowing
;   overlapping executions (parallel computing).
;
;   It is very important to note that there some limitations:
;   - The cpu_process_manager cannot be used in the IDL virtual machine. 
;   - Object references cannot be transferred between processes. 
;   - Only procedures, functions and batch files are allowed to be used with the library. Object methods cannot be called.
;   - From the main session, all data is copied in the child processes, which may affect the performance of your 
;     application if the amount of data involved is large. The use of shared memory segments may offer a more efficient
;     way to share data. 
;     
;     Warning: Incorrect use of shared memory routines can corrupt or even crash your IDL application. proper use of
;     these low level operating system features requires systems programming experience, and is not recommended for 
;     those without such experience. 
;
; @example
;   The sample code below contains the complete communications skeleton for a dynamically load balanced master/slave
;   application. Following the code is a description of the few functions necessary to write typical parallel applications
;   using the cpu_process_manager library. 
;
;   The following program add_value is the function to be executed n times and to be parallelized:
;   
;   function add_value, a
;     wait, .1 ; Represents some time of computation
;     return, a+1
;   end
;
;   The following routine is the commented example:
;   
;   pro exemple_cpu_pm
;     array=indgen(8,8)
;     ; Result
;     array_1=array
;     array_2=array
;     
;     ; Serial computing case
;     tt=systime(/sec)
;     for i=0, n_elements(array)-1 do array_1[i]=add_value(array[i])
;     tt1=systime(/sec)-tt
;     
;     ; Parallel computing case
;     tt=systime(/sec)
;     n_cpu=8l
;     ; Initialize and get the process manager
;     cpu_tm=get_cpu_process_manager(n_cpu,/verbose,wait_time=0.05)
;
;     ; Setup by setting to all sessions to the same working directory, IDL path and compiling the same routines as the
;     ; main IDL session     
;     cpu_tm->setup
;  
;     ; Send one unit of work to each session
;     for i=0, n_cpu-1 do task_id=cpu_tm->call_function('add_value',array[i])
;
;     ; Loop over getting new work requests until there is no more work to be done
;     for i=n_cpu, n_elements(array)-1 do begin
;       ; Wait until one of the processes finish
;       task_id=cpu_tm->waitend()
;       ; Receive result and save it
;       array_2[task_id]=cpu_tm->getresult(task_id)
;       ; Send a new work unit
;       task_id=cpu_tm->call_function('add_value',array[i])
;     endfor
;  
;     ; There is no more work to be done, so receive all the outstanding results from the slaves
;     for i=0, n_cpu-1 do begin
;       task_id=cpu_tm->waitend()
;       array_2[task_id]=cpu_tm->getresult(task_id)
;     endfor
;  
;     tt2=systime(/sec)-tt
;     
;     ; Compare computation time, serial vs parallel
;     print, 'standard time ', tt1
;     print, 'cpu_pm time   ', tt2
;  
;     ; Compare the result (the difference shoule be zero)
;     print, total(abs(array_1 - array_2))
;  end 
;
; @requires 
;   xidl_idlbridge
;
; @history 
;   Created by Bernat Puigdomenech Treserras November 2014
;   
; ***************************************************************************;
;   Copyright (c) 2014 Bernat Puigdomenech Treserras (bernatp3rs@gmail.com)  ;
;                                                                            ;
;   This program is free software: you can redistribute it and/or modify     ;
;   it under the terms of the gnu general public license as published by     ;
;   the free software foundation, either version 3 of the license, or        ;
;   (at your option) any later version.                                      ;
;                                                                            ;
;   This program is distributed in the hope that it will be useful,          ;
;   but without any warranty; without even the implied warranty of           ;
;   merchantability or fitness for a particular purpose.  see the            ;
;   gnu general public license for more details.                             ;
;                                                                            ;
;   You should have received a copy of the gnu general public license        ;
;   along with this program.  if not, see <http://www.gnu.org/licenses/>     ;
; ***************************************************************************;
;-

;+
; cpu_process_manager::init function method initializes the cpu process manager object
; 
; @param n_cores an integer scalar defining the maximum number of bridges (sessions) that the library will create to
;   manage and perform the computations in parallel. the default is to create !cpu.hw_ncpu bridges
; @keywords any property listed under cpu_process_manager properties. to initialize  the value of a property, specify
;   the property name as a keyword set equal to the appropriate property value
;-
function cpu_process_manager::init, n_cores, debug=debug, multithread=multithread, output_path=output_path, wait_time=wait_time, verbose=verbose

  if n_elements(wait_time) eq 0 then wait_time=0.1
  self.wait_time=wait_time > 0.001
  
  if n_elements(verbose) ne 0 then self.verbose=verbose
  
  if n_elements(n_cores) ne 0 then self.n_cores=n_cores>1 $
  else self.n_cores = !cpu.hw_ncpu
  
  if self.verbose then print, 'n cpu', self.n_cores
  
  if n_elements(debug) eq 0 then debug=0L
  ;; idl_idlbridge output property supplying the path and name of a file to which messages generated
  ;; in the child process will be written
  if keyword_set(debug) then begin
    if n_elements(output_path) eq 0 then begin
      output_path=file_dirname(routine_filepath('get_cpu_process_manager',/is_function))
      output_path=filepath('debug',root=output_path)
    endif
    
    if file_test(output_path,/dir) eq 0 then file_mkdir, output_path
  endif
  
  self.idl_bridges=obj_new('idl_container')
  
  for i=0, self.n_cores-1 do begin
    ;; debug file associated to the bridge
    if keyword_set(debug) then begin
      debug_file=filepath('debug_bridge_'+string(i,format='(i4.4)')+'.txt',root=output_path)
      if file_test(debug_file) then file_delete, debug_file, verbose=self.verbose
    endif
      
    new_bridge=obj_new('xidl_idlbridge', multithread=multithread, verbose=self.verbose, $
      wait_time=self.wait_time, output=debug_file) ;callback='cpu_process_end'
    self.idl_bridges->add, new_bridge
  endfor
  
  self.idl_taskref=ptr_new(replicate(-1l,self.n_cores),/no_copy)
  
  defsysv, '!cpu_process_manager', self
  
  self.n_tasks=0L
  
  return, 1
end

;+
; cpu_process_manager::setup procedure method prepares the session of all bridges and reset the task_id counter
;   preparing a session for each bridge implies :
;   1) set the path to the current working directory of the main session
;   2) set the same idl path
;   3) compile the same procedures and functions than the compiled ones in the main session 
;-
pro cpu_process_manager::setup

  for i=0, self.n_cores-1 do begin
    curr_bridge=self.idl_bridges->get(position=i)
    
    ;; abort the command
    if ~curr_bridge->isavailable() then curr_bridge->abort ;curr_bridge->waitend
    ;; wait until the bridge is available to execute the statement
    curr_bridge->prepare_session, /reset
  endfor
  
  ;; reset task_id counter
  self.n_tasks=0L
end

;+
; cpu_process_manager::call_procedure calls the function specified by name, passing any additional parameters as its
;   input and/or output arguments. all documented input and/or output keyword arguments for the called function can
;   also be passed, and they will be treated exactly as if it had been called directly. 
;
; @param name a string containing the name of the function to be called
; @param  pi the position arguments to be passed to the function given by name
; @keyword _extra any keyword handled by the function specified by name 
; @returns returns a longword integer value specifying the task identifier related to the executed process
;-
function cpu_process_manager::call_function, name, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, _ref_extra=extra
  
  on_error, 2
  
  n_param=n_params()
  if n_param lt 1 then message, 'incorrect number of arguments'
  
  bridge=self->getbridge_idle(position=position)
  ;; do not allow to use a bridge which result is not yet retrieved
; if (*self.idl_taskref)[position] ne -1l then bridge->delete_allvar
  
  n_param--
  
  ;; wrapper
  wrapper='bridge->call_function,'''+name+''''
  if n_param gt 0 then wrapper+=','+strjoin('p'+string(1+indgen(n_param),format='(i0)'),',')
  
  if n_elements(extra) ne 0 then wrapper+=',_ref_extra=extra'
  
  aux=execute(wrapper,1,1)
  
  ;; assign new task
  new_task=self->newtask()
  (*self.idl_taskref)[position]=new_task
  
  if self.verbose then $
    print, 'new task '+string(new_task,format='(i0)')+' on bridge '+string(position,format='(i0)')+' - '+systime()
  
  return, new_task
end

;+
; cpu_process_manager::call_procedure calls the procedure specified by name, passing any additional parameters as its
;   input and/or output arguments. all documented input and/or output keyword arguments for the called procedure can
;   also be passed, and they will be treated exactly as if it had been called directly. 
;
; @param name a string containing the name of the procedure to be called
; @param  pi the position arguments to be passed to the procedure given by name
; @keyword _extra any keyword handled by the procedure specified by name 
; @returns returns a longword integer value specifying the task identifier related to the executed process
;-
function cpu_process_manager::call_procedure, name, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, _ref_extra=extra
    
  on_error, 2
  
  n_param=n_params()
  if n_param lt 1 then message, 'incorrect number of arguments'
  
  bridge=self->getbridge_idle(position=position)
  ;; do not allow to use a bridge which result is not yet retrieved
; if (*self.idl_taskref)[position] ne -1l then bridge->delete_allvar
  
  n_param--
  
  ;; wrapper
  wrapper='bridge->call_procedure,'''+name+''''
  if n_param gt 0 then wrapper+=','+strjoin('p'+string(1+indgen(n_param),format='(i0)'),',')
  
  if n_elements(extra) ne 0 then wrapper+=',_ref_extra=extra'
  
  aux=execute(wrapper,1,1)
  
  ;; assign new task
  new_task=self->newtask()
  (*self.idl_taskref)[position]=new_task
  
  if self.verbose then $
    print, 'new task '+string(new_task,format='(i0)')+' on bridge '+string(position,format='(i0)')+' - '+systime()
  
  return, new_task
end

;+
; cpu_process_manager::call_batch method is used to execute a batch file in a child process, in the same manner 
;   as the current idl session
; 
; @param file a string containing the name of the batch file to be executed
; @returns returns a longword integer value specifying the task identifier related to the executed process
;-
function cpu_process_manager::call_batch, file

  on_error, 2
  
  if n_elements(file) eq 0 then message, 'incorrect number of arguments'
  
  bridge=self->getbridge_idle(position=position)
  ;; do not allow to use a bridge which result is not yet retrieved
; if (*self.idl_taskref)[position] ne -1l then bridge->delete_allvar
  
  bridge->call_batch, file
  
  ;; assign new task
  new_task=self->newtask()
  (*self.idl_taskref)[position]=new_task
  
  if self.verbose then $
    print, 'new task '+string(new_task,format='(i0)')+' on bridge '+string(position,format='(i0)')+' - '+systime()
  
  return, new_task
end

;+
; cpu_process_manager::getbridge returns an array of objects references to the bridges in the container. unless the
;   task_id argument, all or position keywords are specified, the first bridge in the container is returned
;   
; @param task_id the task identifier or an array of task identifiers related to the bridge(s) to return
; @keyword all  set this keyword to return an array of object references to all bridges. if this keyword is set, 
;   the task_id argument is not required
; @keyword position set this keyword equal to a scalar or array containing the zero-based indices of the positions
;   of the bridges to return. if the task_id argument is supplied, this keyword is ignored  
; @keyword count set this keyword equal to a named variable that will contain the number of objects selected by 
;   the function
;-
function cpu_process_manager::getbridge, task_id, all=all, position=position, count=count

  on_error, 2
  
  if ~keyword_set(all) then begin
    
    ntask_id=n_elements(task_id)
    if ntask_id ne 0 then begin
      
      bridges=objarr(ntask_id)
      for i=0, ntask_id-1 do begin
        ww=where(*self.idl_taskref eq task_id[i],nn_w)
        if nn_w eq 0 then message, 'task id value not associated to any of the bridges'
        
        bridges[i]=self.idl_bridges->get(position=ww[0])
      endfor
      
      if arg_present(count) then count=long(total(obj_valid(bridges)))

    endif else begin
      
      if n_elements(position) ne 0 then bridges=self.idl_bridges->get(position=position,count=count) $
      else bridges=self.idl_bridges->get(count=count)
    
    endelse
      
  endif else begin
  
    if n_elements(position) ne 0 then message, 'keywords all and position are mutually exclusive'
    bridges=self.idl_bridges->get(/all,count=count)
  
  endelse
    
  return, bridges
end

;function cpu_process_manager::getbridge, task_id, all=all, count=count
;
;  if ~keyword_set(all) then begin
;      
;    ww=where(*self.idl_taskref eq task_id,count)
;    if count gt 0 then bridges=self.idl_bridges->get(position=ww) $
;    else bridges=-1l
;      
;  endif else bridges=self.idl_bridges->get(/all,count=count)
;    
;  return, bridges
;end

;+
; cpu_process_manager::getbridge_idle returns the first bridge that does not have an associated task (is idle)
; 
; @keyword position set this keyword equal to a named variable that will contain the position at which the 
;   bridge is located within the container
;-
function cpu_process_manager::getbridge_idle, position=position

  on_error, 2

  ww=where((*self.idl_taskref) eq -1l,nn_w)
  if nn_w ge 1 then begin
  
    position=ww[0]
    curr_bridge=self.idl_bridges->get(position=position)
    
  endif else message, 'all available bridges are currently in use. get results before assigning new tasks'
  
  return, curr_bridge
end

;+
; cpu_process_manager::waitend function method returns the first task identifier associated to a bridge that
;   has already completed the execution of the task. if all bridges are idle and do not have an associated 
;   task, the execution will return to the caller, then stop and print an error message 
;
; @return a task identifier of the a finished process
;-
function cpu_process_manager::waitend

  ;; todo wait all

  ; if the user wants to waitend an specific task do:
  ;    bridge=cpu_pm->getbridge(task_id)
  ;    bridge->waitend 

  on_error, 2

  ww=where((*self.idl_taskref) ne -1l,nn_w)
  if nn_w ge 1 then begin
  
    while 1 do begin
    
      for i=0, nn_w-1 do begin
        position=ww[i]
        curr_bridge=self.idl_bridges->get(position=position)
        
        if curr_bridge->isavailable() then begin
          done=1
          break
        endif
      endfor
      
      if keyword_set(done) then break $
      else wait, self.wait_time
      
    endwhile
    
    task_id=(*self.idl_taskref)[position]
    
  endif else message, 'all bridges are idle and do not have an associated task'
  
  return, task_id
end

;+
; cpu_process_manager::reset_taskcount procedure method sets the task identifier counter to zero
;-
pro cpu_process_manager::reset_taskcount
  
  ;; with great power comes great responsability
  self.n_tasks=0L
end

;+
; cpu_process_manager::newtask function method returns a new and unique task identifer. the internal task counter
;   will be increased by one. if the task counter reaches 2^31-1, it will be reset
; 
; @return a 32-bit integer task identifier
; @private
;-
function cpu_process_manager::newtask

  compile_opt hidden

  task_id=self.n_tasks
  if self.n_tasks eq 2L^31-1 then self.n_tasks=0L else self.n_tasks++ ;; prevent 32-bit integer overflow
  
  return, task_id
end

;+
; cpu_process_manager::execute_all procedure method executes an idl command to all child processes. every bridge 
;   will execute the command once it will be available. if a bridge is executing a process, the method will wait
;   until tit becomes available. 
;
;   there are two possible modes in which this can be done:
;   - synchronous — idl waits until the child process completes the specified operation before to return. this is the 
;     default
;   - asynchronous —the call returns immediately, and the caller can use status method to track the progress of all
;     bridges.
; 
; see the idl_idlbridge::execute help page for more information   
; 
; @param idlstmt a string containing an idl command to be executed by all child idl processes
; @keyword nowait set this keyword to cause the return immediately, rather than waiting for every child process to
;   complete. when nowait is set, the parent idl process continues to execute in parallel with the child processes.
;-
pro cpu_process_manager::execute_all, idlstmt, nowait=nowait

  for i=0, self.n_cores-1 do begin
  
    curr_bridge=self.idl_bridges->get(position=i)
    ;; wait until the bridge is available to execute the statement
    if ~curr_bridge->isavailable() then curr_bridge->waitend
    
    curr_bridge->execute, idlstmt, nowait=nowait
  endfor
end

;+
; cpu_process_manager::getresult function method retrieves the result of the process identified by task_id. if the 
;   process is not finished, the bridge will wait until its process is completed. all additional output parameters
;   and keywords previsouly passed to cpu_process_manager::call_function or call_procedure can be supplied to 
;   retrieve the result
; 
; @param task_id the task identifier of the process which its result is requested  
; todo remeber that some definitions are the same in xidl_idlbridge. keep the right documentation to be consistent
; @param p1..pN output parameters 
; @keyword .. output keywords

; @returns the result of the called function or 1 if the executed process was a procedure or a batch file
;-
function cpu_process_manager::getresult, task_id, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, _ref_extra=extra
    
  ;; todo for procedures the user should not be forced to get the result
  ;;      on_callback if it is procedure (*self.idl_taskref)[ww]=-1l but attention with waitend
    
  on_error, 2
  
  ww=where(*self.idl_taskref eq task_id,nn_w)
  ;; the task is not referenced
  if nn_w eq 0 then message, 'the task number '+string(task_id,format='(i0)')+' is not associated to any bridge'
  
  if self.verbose then $
    print, 'get result from task '+string(task_id,format='(i0)')+' on bridge '+string(ww[0],format='(i0)')
  
  curr_bridge=self.idl_bridges->get(position=ww[0])
  
  result=curr_bridge->getresult(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, _ref_extra=extra)
  
  ;; do not reset, keep directory and compiled programs
;  curr_bridge->reset
  
  (*self.idl_taskref)[ww]=-1l
  ;; delete all variables
  curr_bridge->delete_allvar
  
  return, result
end

;+
; cpu_process_manager::getproperty procedure method retrieves a property or group of properties for this object
;-
pro cpu_process_manager::getproperty, n_cores=n_cores, verbose=verbose, wait_time=wait_time
  if arg_present(n_cores) then n_cores=self.n_cores
  if arg_present(verbose) then verbose=self.verbose
  if arg_present(wait_time) then wait_time=self.wait_time
end

;+
; cpu_process_manager::setproperty procedure method sets the properties for this object and all contained bridges
;-
pro cpu_process_manager::setproperty, verbose=verbose, wait_time=wait_time

  if n_elements(verbose) ne 0 then begin
    self.verbose=verbose
    for i=0, self.n_cores-1 do (self.idl_bridges->get(position=i))->setproperty, verbose=self.verbose
  endif
  
  if n_elements(wait_time) ne 0 then begin
    self.wait_time=wait_time > 0.0001
    for i=0, self.n_cores-1 do (self.idl_bridges->get(position=i))->setproperty, wait_time=self.wait_time
  endif
end

;+
; cpu_process_manager::status function method queries the state of all bridges in the container. the possible values
;   indicating the status of the bridge are:
;
;   return value          description what                     error keyword returns
;        0                     idle                                 null string
;        1                executing command                         null string
;        2                completed command                         null string
;        3            error halted execution                  descriptive error string
;        4      aborted execution (idl_idlbridge::abort)           abort message 
;
;  descriptive error messages are returned in the variable specified by the error keyword, if it's the case
;
; @keyword error a named variable that will contain a string value corresponding to the error that occurred while 
;   executing the command in the child process, if any. 
;
; @keyword all set this keyword to return all available information about the whole status of the library in a structure.
;   the following are descriptors of the fields in the returned structure:
;
;   field               description
;   n_cores         the number of bridges (sessions) managed by the library
;   busy            the number of busy bridges 
;   available       the number of available bridges
;   status          the status of each bridge
;   error           the descriptive error message occurred while execution in each bridge, if any.
;   task_id         the task identifier associated to each bridge 
;-
function cpu_process_manager::status, error=error, all=all

  status=lonarr(self.n_cores,/nozero)
  error=strarr(self.n_cores)
  
  for i=0, self.n_cores-1 do begin
    curr_bridge=self.idl_bridges->get(position=i)
    
    status[i]=curr_bridge->status(error=curr_error)
    error[i]=curr_error
  endfor
  
  if keyword_set(all) then begin
  
    busy=long(total(status eq 1))
    available=self.n_cores - busy
  
    result={ n_cores: self.n_cores, busy: busy, available: available, status: temporary(status), $
      error: temporary(error), task_id: (*self.idl_taskref) }
  
  endif else result=temporary(status)
  
  return, result
end

;+
; cpu_process_manager::killall aborts the execution of all bridges and resets theirs sessions 
;-
pro cpu_process_manager::killall
  
  for i=0, self.n_cores-1 do begin
    curr_bridge=self.idl_bridges->get(position=i)
    curr_bridge->reset
  endfor
  
end

;+
; cpu_process_manager::cleanup procedure method performs all cleanup when the cpu manager object is destroyed
;   all bridges are destroyed. the following steps will occur for every bridge:
;
;   if there is a currently executing a command, then idl_idlbridge::abort is called
;   if there is a pending callback, then the idl_idlbridge::oncallback method (and the callback procedure) is called
;   the child idl process is destroyed
;-
pro cpu_process_manager::cleanup
  if self.verbose then print, 'deleting cpu_process_manager...'

  ptr_free, self.idl_taskref
  
  obj_destroy, self.idl_bridges
end

;+
; cpu_process_manager class definition
;
;   @properties objects of this class have the following properties:
;
;   @property wait_time a floating-point value specifying the duration time, in seconds, that the cpu manager and all 
;     bridges use to suspend the execution before querying (in an infinite-loop) the state of the process until it is
;     completed, aborted or finished due an error. this time could not be less than 0.0001 seconds. the default value
;     is 0.1. note that setting a small wait time may affect the performance of your application because resting in a 
;     loop querying and waiting the bridges also consumes cpu,
;   @property verbose the keyword verbose causes the cpu manager and all bridges to issue informative messages about 
;     the internal operations that are going on.
;
;   @field idl_bridges the container for all bridges
;   @field idl_taskref a pointer to array of task identifiers associated to every bridge
;   @field n_cores the number of bridges (sessions) that the library manages
;   @field verbose the flag to print informative messages if it is set to 1
;   @field wait_time the suspending time, in seconds
;-
pro cpu_process_manager__define

  struct = { cpu_process_manager, $
    idl_bridges: obj_new(), $
    idl_taskref: ptr_new(), $
    n_cores: 0, $ 
    n_tasks: 0l, $
    ; cpu_process_manager options
    verbose: 0, $
    wait_time: 0. }
    
end

;+
; get_cpu_process_manager function is used to initialize and get the cpu_process_manager. the first call to the function will 
;   create the cpu manager using the input optional argument n_cores and any property, via keyword, listed under cpu_process_manager
;   properties. this initialization will be done only once.
;   after the initialization, get_cpu_process_manager will return the reference to the cpu_process_manager object that allows the
;   control of the slave sessions.  
;   restricting the instantiation of this class to one object is very useful when exactly one object is needed to coordinate actions 
;   across the system.
;   
;   @param n_cores a integer scalar defining the maximum number of bridges (sessions) that the library will create to
;     manage and perform the computations in parallel. the default is to create !cpu.hw_ncpu bridges
;   @keywords any property listed under cpu_process_manager properties. to initialize  the value of a property, specify
;     the property name as a keyword set equal to the appropriate property value
;     
;     debug, output_path and multithread are properties that can only be specified via the Init method 
;     
;   @return the object reference to the cpu_process_manager library
;-
function get_cpu_process_manager, n_cores, debug=debug, output_path=output_path, multithread=multithread, _extra=extra

  on_error, 2

  defsysv, '!cpu_process_manager', exists=exists
  
  if exists eq 0 then begin
    instance=obj_new('cpu_process_manager', n_cores, debug=debug, output_path=output_path, multithread=multithread, _strict_extra=extra)
    defsysv, '!cpu_process_manager', instance
  endif else begin
    if obj_valid(!cpu_process_manager) then begin
      instance=!cpu_process_manager
      
      instance->getproperty, n_cores=n_cores_get
      if n_elements(n_cores) gt 0 then begin  
        ;; todo allow changing the number of cores
        if n_cores gt n_cores_get then message, 'the number of system processors cannot be modified'
      endif else n_cores=n_cores_get
      
      if n_elements(extra) ne 0 then !cpu_process_manager->setproperty, _strict_extra=extra
    endif else begin
      instance=obj_new('cpu_process_manager', n_cores, debug=debug, output_path=output_path, multithread=multithread, _strict_extra=extra)
      
      if size(!cpu_process_manager,/type) eq 11 then !cpu_process_manager=instance $
      else message, 'the system variable !cpu_process_manager already exists and is not an object type', /continue 
    endelse
  endelse
  
  return, instance
end