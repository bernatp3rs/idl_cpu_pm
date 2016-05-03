;+
; @name 
;   xidl_idlbridge
; 
; @author 
;   bernat puigdomenech treserras
;   e-mail: bernat@
;   
; @purpose 
;   the object xidl_idlbridge is an extension of the idl_idlbridge object that, amongs other things, allows to 
;   transfer structures and pointers between processes, work with shared memory variables, call procedures and
;   functions aynchronously and get information about the state of the session in an easy way. 
;   you can use the xidl_idlbridge object separately from the main library cpu_process_manager if you find it more 
;   useful.  
;
; @history 
;   created by bernat puigdomenech treserras november 2014
;   
; ***************************************************************************;
;   copyright (c) 2014 bernat puigdomenech treserras (bernatp3rs@gmail.com)  ;
;                                                                            ;
;   this program is free software: you can redistribute it and/or modify     ;
;   it under the terms of the gnu general public license as published by     ;
;   the free software foundation, either version 3 of the license, or        ;
;   (at your option) any later version.                                      ;
;                                                                            ;
;   this program is distributed in the hope that it will be useful,          ;
;   but without any warranty; without even the implied warranty of           ;
;   merchantability or fitness for a particular purpose.  see the            ;
;   gnu general public license for more details.                             ;
;                                                                            ;
;   you should have received a copy of the gnu general public license        ;
;   along with this program.  if not, see <http://www.gnu.org/licenses/>     ;
; ***************************************************************************;
;-

;+
; xidl_idlbridge::init function method initializes the xidl_idlbridge object. it is important to note that after
;   the creation of the bridge, the command 'cpu, tpool_nthreads=1' is executed in order to disable the use of
;   multithreading for the list of routines that use the thread pool table.
; 
; @keywords any property listed under xidl_idlbridge properties. to initialize  the value of a property, specify
;   the property name as a keyword set equal to the appropriate property value
;-
function xidl_idlbridge::init, output=output, _extra=extra

  res=self->idl_idlbridge::init(output=output)
  
  ;; todo get multi-platform process id 
  if !version.os eq 'linux' then begin
    self->idl_idlbridge::execute, 'pid=call_external(!dlm_path+''/libidl.so'',''getpid'',/cdecl)'
    self.pid=self->idl_idlbridge::getvar('pid')
    self->idl_idlbridge::execute, 'delvar, pid'
  endif else self.pid=-1 
  
  self.wait_time=0.1
  if n_elements(extra) ne 0 then self->setproperty, _strict_extra=extra
  
  if ~self.multithread then self->execute, 'cpu, tpool_nthreads=1'
  
  ;; the startup file is not automatically executed when the idl_idlbridge is created 
;  file_startup=pref_get('idl_startup')
;  if file_test(file_startup) then self->execute, '@'+file_startup 
  
  return, res
end

;+
; xidl_idlbridge::call_function procedure method
; 
; @param name a string containing the name of the function to be called
;-
pro xidl_idlbridge::call_function, name, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, _ref_extra=extra
    
  on_error, 2
  if n_elements(name) eq 0 then message, 'incorrect number of arguments'
  
  n_param=n_params()-1
  
  ;; wrapper
  wrapper='self->processroutine,'''+name+''',1l'
  if n_param gt 0 then wrapper+=','+strjoin('p'+string(1+indgen(n_param),format='(i0)'),',')
  
  if n_elements(extra) ne 0 then wrapper+=',_ref_extra=extra'
  
  aux=execute(wrapper,1,1)
end

;+
; xidl_idlbridge::call_procedure procedure method
; 
; @param name a string containing the name of the procedure to be called
;-
pro xidl_idlbridge::call_procedure, name, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, _ref_extra=extra
    
  on_error, 2
  if n_elements(name) eq 0 then message, 'incorrect number of arguments'
  
  n_param=n_params()-1
  
  ;; wrapper
  wrapper='self->processroutine,'''+name+''',0l'
  if n_param gt 0 then wrapper+=','+strjoin('p'+string(1+indgen(n_param),format='(i0)'),',')
  
  if n_elements(extra) ne 0 then wrapper+=',_ref_extra=extra'
  
  aux=execute(wrapper,1,1)
end

;+
; xidl_idlbridge::call_batch procedure method
; 
; @param file a string containing the name of the batch file to be executed 
;-
pro xidl_idlbridge::call_batch, file
  
  on_error, 2
  if n_elements(file) eq 0 then message, 'incorrect number of arguments'
  
  self->waitend
  
  self->execute, '@'+file, /nowait
end

;+
; xidl_idlbridge::processroutine procedure methos
;
;   @private
;-
pro xidl_idlbridge::processroutine, routine_name, is_function, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, _ref_extra=extra
    
  compile_opt hidden
    
  ;; wait for the bridge to be available
  self->waitend
  
  ;; todo what if the result is not retrieved before processing a new procedure/function
  ;; the result may be overwritten. note that cpu_pm prevents that 
  
  n_param=n_params()-2
  n_extra=n_elements(extra)
  
  if is_function then begin
    self.exec_info.res_var=self->temporaryname()
    exec_stmt=self.exec_info.res_var+'='+routine_name+'('
  endif else begin
    self.exec_info.res_var=''
    exec_stmt=routine_name
  endelse
  
  if n_param gt 0 then begin
    var_info=self->processvar(n_param,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)

    if ~is_function then exec_stmt+=','
    exec_stmt+=strjoin(var_info.name,',')
  endif
  
  if n_extra gt 0 then begin
    key_info=self->processkey(n_extra,_ref_extra=extra)
    
    if ~is_function or n_param gt 0 then exec_stmt+=','
    exec_stmt+=strjoin(extra+'='+key_info.name,',')
  endif
  
  if is_function then exec_stmt+=')'
  
  self->execute, exec_stmt, /nowait
end

;+
; xidl_idlbridge::
;
; @private
;-
function xidl_idlbridge::processvar, n_param, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20
    
  compile_opt hidden 
   
  var_info=replicate({name:'',input:0},n_param)
  
  for i=0, n_param-1 do begin
    i_param='p'+string(i+1,format='(i0)')
    
    aux=execute('input=n_elements('+i_param+') ne 0',1,1)
    if input then aux=execute('name=self->setparam('+i_param+')',1,1) $ ;; input var
    else name=self->temporaryname()                                     ;; output var
    
    var_info[i].name=name
    var_info[i].input=input
  endfor
  
  ptr_free, self.exec_info.var
  self.exec_info.var=ptr_new(var_info)
  
  return, var_info
end

;+
; xidl_idlbridge::processkey function method 
;
; @private
;-
function xidl_idlbridge::processkey, n_extra, _ref_extra=extra

  compile_opt hidden

  key_info=replicate({keyname:'',name:'',input:0},n_extra)
  
  scope_level=-2
  scope_trace=scope_traceback(/structure)
  ;; called from cpu_process_manager
  prev_call=scope_trace[n_elements(scope_trace)-4].routine
  if stregex(prev_call,'cpu_process_manager.*',/boolean,/fold_case) then scope_level--
  
  for i=0, n_extra-1 do begin
    input=n_elements(scope_varfetch(extra[i],/ref_extra,level=scope_level)) ne 0
    
    ;; input/output parameter
    if input then begin
      value=scope_varfetch(extra[i],/ref_extra,level=scope_level)
      name=self->setparam(value,/tname) ;; use tname. scope_varname cannot retrieve the name of the variable passed by keyword
    endif else name=self->temporaryname()
    
    key_info[i].keyname=extra[i]
    key_info[i].name=name
    key_info[i].input=input
  endfor
  
  ptr_free, self.exec_info.key
  self.exec_info.key=ptr_new(key_info)
  
  return, key_info
end

;+
; xidl_idlbridge::setparam
;
; @keyword tname set this keyword to force the parameter to get a temporary name in the child session
; 
; @private
;-
function xidl_idlbridge::setparam, param, ref_extra=ref_extra, tname=tname

  compile_opt hidden
  
  if ~keyword_set(tname) then begin
    scope_level=-4
    scope_trace=scope_traceback(/structure)
    ;; called from cpu_process_manager
    prev_call=scope_trace[n_elements(scope_trace)-5].routine
    if stregex(prev_call,'cpu_process_manager.*',/boolean,/fold_case) then scope_level--
    
    name=scope_varname(param,level=scope_level)
    ;; scope_varname can return null if the paramater is an expression, constant, system variable
    ;; and subscripted variable reference (pe. !data, data.data, 1, data[4], etc.)
    ;; = parameter passed by value
    ;; entire structures are passed by reference, but individual structure fields are passed by value.
    if ~keyword_set(name) then name=self->temporaryname()
  endif else name=self->temporaryname()
  
  case size(param,/type) of
    ;; undefined / output parameter
    0:
    
    ;; structure
    8: begin
      n_elem=n_elements(param)
      
      tname=self->temporaryname()
      tagnames=tag_names(param[0])
      
      ;; use tname to avoid overwriting existing parameters
      for i=0, n_elem-1 do begin
      
        ;; todo avoid concatenation
        for j=0, n_elements(tagnames)-1 do begin
          jname=self->setparam(param[i].(j),/tname)
          
          if j eq 0 then self->idl_idlbridge::execute, tname+'=create_struct('''+tagnames[j]+''',temporary('+jname+'))' $
          else self->idl_idlbridge::execute, tname+'=create_struct(temporary('+tname+'),'''+tagnames[j]+''',temporary('+jname+'))
        endfor
        
        if i eq 0 then self->idl_idlbridge::execute, name+'=temporary('+tname+')' $
        else self->idl_idlbridge::execute, name+'=[temporary(name),temporary('+tname+')'
        
      endfor
    end
    
    ;; pointer
    10: begin
      n_elem=n_elements(param)
      self->idl_idlbridge::execute, name+'=ptr_new('+string(n_elem,format='(i0)')+')'
      
      for i=0, n_elem-1 do if ptr_valid(param[i]) then begin
        iname=self->setparam(*param[i],/tname)
        self->idl_idlbridge::execute, name+'['+string(i,format='(i0)')+']=ptr_new('+iname+',/no_copy)'
      endif
    end
    
    ;; object
    ;; todo error handler
    11: message, 'unsupported parameter type: objref'
    
    ;; scalar or array variable of numeric or string type
    else: begin
      
      help, param, output=help_out
      
      help_out=strlowcase(help_out)
      
      shm_pos=strpos(help_out[0],'sharedmemory')
      shm_res=shm_pos ne -1
  
      ;; shared memory segment
      if shm_res then begin

        segname=stregex(help_out[0],'<.*>',/extract)
        segname=strmid(segname,1,strlen(segname)-2)
        
        sz=size(param)
        sz='['+strjoin(string(sz,format='(i0)'),',')+']'
        
        self->idl_idlbridge::execute, 'shmmap, '''+segname+''',size='+sz+',/destroy_segment'
        self->idl_idlbridge::execute, name+'=shmvar('''+segname+''')'
        
        print, 'shmmap, '''+segname+''',size='+sz+',/destroy_segment'
        
;        self->execute, 'shmmap, '''+segname+''',size='+sz+',/destroy_segment'
;        self->execute, name+'=shmvar('''+segname+''')'
;        self->execute, 'print, '+name
          
      endif else self->setvar, name, param
      
    end
    
  endcase
  
  return, name
end

;+
; xidl_idlbridge::temporaryname function method returns a scalar string with a temporary name based on the current
;   system time
;
; @returns a scalar string defining a temporary and unique name
;-
function xidl_idlbridge::temporaryname
  ;; todo may find another way to create variable names 
  return, 'tempvar_'+strjoin(strsplit(string(systime(/sec),format='(f0.6)'),'.',/extract),'_')
end

;+
; xidl_idlbridge::waitend procedure method halts the execution until the bridge becomes available. the property
;   wait_time is used to suspend the execution between each request to get the status of the bridge  
;-
pro xidl_idlbridge::waitend

  while 1 do begin
    if ~self->isavailable() then wait, self.wait_time $
    else break
  endwhile
  
end

;+
; xidl_idlbridge::isavailable function method checks the status of the bridge and return 1 if the bridge is not 
;   executing any process
; 
; @returns 0 if the bridge is executing a process, 1 otherwise 
;- 
function xidl_idlbridge::isavailable

  return, (self->status() ne 1)
end

;+
; xidl_idlbridge::execute procedure method overwrites idl_idlbridge::execute by printing the execution string command
;   if the verbose property is set to 1. after that, the given idl command is executed to the child process. there are
;   two possible modes in which this can be done:
;
;   - synchronous — idl waits until the child process completes the specified operation before to return. this is the 
;     default
;   - asynchronous —the call returns immediately, and the caller can use status method to track the progress of the bridge
;
; see the idl_idlbridge::execute help page for more information   
; 
; @param idlstmt a string containing an idl command to be executed by the child idl process
; 
; @keyword nowait set this keyword to cause the bridge to return immediately, rather than waiting for the child process 
; to complete. when nowait is set, the parent idl process continues to execute in parallel with the child process.
; 
; @overwrites idl_idlbridge::execute
;-
pro xidl_idlbridge::execute, idlstmt, nowait=nowait

  if self.verbose then print, idlstmt ;; debugging purposes
  self->idl_idlbridge::execute, idlstmt, nowait=nowait
end

;+
; xidl_idlbridge::cd procedure method is used to set and/or change the current working directory of the child process.
;   note that, automatically, calling the procedure cd on the main session changes the working directory of all child
;   processes started from idl during that session. under unix, cd does not affect the child processes. 
; 
; @param path a string containing the name of the directory to change to. the current directory is pushed onto the top
;   of the directory stack.
; @keyword current set this keyword to a named variable into which the current working directory is stored. the 
;   returned directory is the working directory before the directory is changed. 
;-
pro xidl_idlbridge::cd, path, current=current
  
  self->waitend
  
  command='cd, '''+path+'''
  
  if arg_present(current) then begin
    current_name=self->temporaryname()
    command+=', current='+current_name
  endif

  self->execute, command
  
  if arg_present(current) then begin
    current=self->idl_idlbridge::getvar(current_name)
    self->idl_idlbridge::execute, 'delvar, '+current_name
  endif
  
end

;+
; xidl_idlbridge::reset procedure methos executes the .reset_session command. this command resets much of the 
;   state of the child idl session. see the .reset_session entry in the idl help for more details. after reseting 
;   the session, the command "cpu, tpool_nthreads=1" is also executed in order to disable the use of multithreading
;   for the list of routines that use the thread pool table.
;-
pro xidl_idlbridge::reset

  if ~self->isavailable() then self->abort 

  self->execute, '.reset_session'
  ;; 1 thread while performing computation
  if ~self.multithread then self->execute, 'cpu, tpool_nthreads=1'
  ;; execute the startup file
  ;; the idl_startup is executed automatically each time idl is started and following a .reset_session or 
  ;; .full_reset_session executive command
;  file_startup=pref_get('idl_startup')
;  if file_test(file_startup) then self->execute, '@'+file_startup 
end

;+
; xidl_idlbridge::prepare_session procedure method prepares the session of the bridge by proceding to trough the 
;   following steps:
;   
;   1) if the keyword reset is set and the process is executing a command, the session is aborted. after that, the 
;   method xidl_idlbridge::reset is called. if the keyword reset is not set, the method waits the bridge to become
;   available (xidl_idlbridge::waitend)
;   2) set the path to the current working directory of the main session
;   3) set the same idl path as the main session
;   4) compile the same procedures and functions than the compiled ones in the main session 
;   
;   @keyword reset if this keyword is set, the session will be reset before starting the setup. otherwise, the method
;     will wait until the bridge becomes available 
;-
pro xidl_idlbridge::prepare_session, reset=reset

  ;; todo keyword silent 
  
  if keyword_set(reset) then begin
    if ~self->isavailable() then curr_bridge->abort 
    self->reset
  endif else self->waitend
  
  ;; path
  cd, '.', curr=path
  self->idl_idlbridge::execute, 'cd, '''+path+'''
;  self->cd, path
  
  ;; idl path
  self->idl_idlbridge::execute, '!path='''+!path+'''
  
  ;; compiled procedures and functions
  proc=routine_info(/source)
  func=routine_info(/source,/functions)
  
  if keyword_set(proc) then begin
    if keyword_set(func) then rout=[temporary(proc),temporary(func)] else rout=temporary(proc)
  endif else begin
    if keyword_set(func) then rout=temporary(func) else return
  endelse
  
  ;; compile only once (object methods make the path to appear several times)
  rout=rout[uniq(rout.path,sort(rout.path))]
  
  for i=0l, n_elements(rout)-1 do if rout[i].path ne '' then begin
    ext=strmid(rout[i].path,strpos(rout[i].path,'.',/reverse_search)+1,99)
    ;; sav files may be included but may not be compiled
    if strlowcase(ext) eq 'pro' then self->idl_idlbridge::execute, '.compile '+rout[i].path
  endif
  
end

;+
; xidl_idlbridge::delete_allvar deletes all variables defined in the child session
;-
pro xidl_idlbridge::delete_allvar
  
  ;; todo test

  all_var=self->temporaryname()

  self->idl_idlbridge::execute, all_var+'=routine_info(''$main$'',/variables)'
  all_var=self->idl_idlbridge::getvar(all_var)
  
  command='delvar, '+strjoin(all_var,', ')
  self->idl_idlbridge::execute, command
end

;+
; xidl_idlbridge::oncallback procedure method overwrites the idl_idlbridge::oncallback method by printing the error
;   message if any have occurred while executing the process. 
;   
; see the idl_idlbridge::execute help page for more information 
; 
; @overwrites idl_idlbridge::oncallback
;-
pro xidl_idlbridge::oncallback, status, error

  ;; error halted or aborted execution
  if status ge 3 and self.verbose eq 1 then print, error

  self->idl_idlbridge::oncallback, status, error
end

;+
; xidl_idlbridge::help process method executes the command help in the child process and retrieves the output to
;   give the user information on many aspects of the child idl session. it prints the current nesting of procedures 
;   and functions, all current variables at the program level, and open files. 
; 
; todo documentation 
;
; @param output set this keyword equal to a named variable that will contain a string array containing the formatted
;   output of the help command. each line of formatted output becomes a single element in the string array. 
;-
pro xidl_idlbridge::help, var_name, output=output

  self->waitend

  help_out=self->temporaryname()
  
  if keyword_set(var_name) then idlstmt='help, '+var_name+', output='+help_out $
  else idlstmt='help, output='+help_out
  
  self->idl_idlbridge::execute, idlstmt
  
  temp=self->idl_idlbridge::getvar(help_out)
  self->idl_idlbridge::execute, 'delvar, '+help_out
  
  if arg_present(output) then output=temporary(temp) else for i=0, n_elements(temp)-1 do print, temp[i]
end

;+
; xidl_idlbridge::memory procedure method gives the user information on the amount of dynamic memory currently in use
;   by the child idl session.
;   
; @keyword kilobyte set this keyword to give the amount of memory expressed in kilobytes. bytes, by default
; @keyword megabyte set this keyword to give the amount of memory expressed in megabytes. bytes, by default
; @keyword gigabyte set this keyword to give the amount of memory expressed in gigabytes. bytes, by default
;-
pro xidl_idlbridge::memory, kilobyte=kilobyte, megabyte=megabyte, gigabyte=gigabyte

  self->waitend

  used_mem=self->temporaryname()
  self->idl_idlbridge::execute, used_mem+'=memory(/current)'

  memory_bridge=self->idl_idlbridge::getvar(used_mem)
  self->idl_idlbridge::execute, 'delvar, '+used_mem

  memory_bridge=memory(/current)
  
  if keyword_set(kilobyte) then used_mem_txt=strtrim(string(memory_bridge/2^10d),2)+' k' else $
  if keyword_set(megabyte) then used_mem_txt=strtrim(string(memory_bridge/2^20d),2)+' m' else $
  if keyword_set(gigabyte) then used_mem_txt=strtrim(string(memory_bridge/2^30d),2)+' g' else $
  used_mem_txt=strtrim(string(memory_bridge),2)+' b'
    
  verbose_text='heap memory used: '+used_mem_txt
  print, verbose_text
end

;+
; xidl_idlbridge::interrupt (linux only)
;   sends an interrupt signal "Ctrl+C" to the session controled by the bridge 
;-
pro xidl_idlbridge::interrupt
  
  if self->idl_idlbridge::status() eq 1 then begin
    if self.pid ne -1 then spawn, 'kill -sigint '+string(self.pid,format='(i0)') $
    else print, 'no such process id to send interrupt signal'
  endif

end

;+
; xidl_idlbridge::get_var function method copies the value of a variable from the child process to the main session.
;   the variable must exist in the child process and can not be an object reference. this method overwrites the
;   xidl_idlbridge::get_var extending the functionality to transfer structures and pointers
;
;   note that if the required variable is defined as a shared memory segment, the returned variable won't be copied
;   from the child session
;
; @param var_name a string containing the name of the variable to be transferred from the child process.
; @returns the value of the variable transferred from the child process.
; 
; @overwrittes xidl_idlbridge::get_var
;-
function xidl_idlbridge::getvar, var_name

  ;; use idl_idlbridge::execute because some actions do not need to be verbosed

  res_name=self->temporaryname()
  self->idl_idlbridge::execute, res_name+'=size('+var_name+',/type)'
  
  res_type=self->idl_idlbridge::getvar(res_name)
  self->idl_idlbridge::execute, 'delvar, '+res_name
  
; if self.verbose then print, var_name, res_type
  
  case res_type of
    ;; undefined / output parameter
    0: result=-1
    
    ;; structure
    8: begin
    
      n_str_name=self->temporaryname()
      self->idl_idlbridge::execute, n_str_name+'=n_elements('+var_name+')'
      n_tag_name=self->temporaryname()
      self->idl_idlbridge::execute, n_tag_name+'=tag_names('+var_name+')'
      
      n_str=self->idl_idlbridge::getvar(n_str_name)
      self->idl_idlbridge::execute, 'delvar, '+n_str_name
      n_tag=self->idl_idlbridge::getvar(n_tag_name)
      self->idl_idlbridge::execute, 'delvar, '+n_tag_name
      
      ;; considering array of structures
      for i=0, n_str[0]-1 do begin
        i_str=strtrim(string(i),1)
        ;; structure tags
        for j=0, n_elements(n_tag)-1 do begin
          j_str=strtrim(string(j),1)
          
          tname=self->temporaryname()
          
          self->idl_idlbridge::execute, tname+'='+var_name+'['+i_str+'].('+j_str+')'
          aux=self->getvar(tname)
          self->idl_idlbridge::execute, 'delvar, '+tname
          
          if keyword_set(tag_result) then tag_result=create_struct(temporary(tag_result),n_tag[j],temporary(aux)) $
          else tag_result=create_struct(n_tag[j],temporary(aux))
        endfor
        
        if ~keyword_set(result) then begin
        
          if n_str gt 1 then begin
            result=replicate(tag_result,n_str)
            result[0]=temporary(tag_result)
          endif else result=temporary(tag_result)
          
        endif else result[i]=temporary(tag_result)
      endfor
    end
    
    ;; pointer
    10: begin
    
      n_ptr_name=self->temporaryname()
      
      self->idl_idlbridge::execute, n_ptr_name+'=n_elements('+var_name+')'
      n_ptr=self->idl_idlbridge::getvar(n_ptr_name)
      self->idl_idlbridge::execute, 'delvar, '+n_ptr_name
      
      for i=0, n_ptr-1 do begin
      
        tname=self->temporaryname()
        
        self->idl_idlbridge::execute, tname+'=*'+var_name+'['+string(i,format='(i0)')+']'
        aux=ptr_new(self->getvar(tname),/no_copy)
        
        self->idl_idlbridge::execute, 'delvar, '+tname
        
        if keyword_set(result) then result=[result,aux] $
        else result=aux
      endfor
    end
    
    ;; object
    11: result=-1
    
    ;; scalar or array variable of numeric or string type
    else: begin
    
      self->help, var_name, output=help_out
      
      shm_pos=strpos(strlowcase(help_out[0]),'sharedmemory')
      shm_res=shm_pos ne -1
  
      ;; shared memory segment
      if shm_res then begin

        segname=stregex(help_out[0],'<.*>',/extract)
        segname=strmid(segname,1,strlen(segname)-2)
        
        tname=self->temporaryname()

        self->idl_idlbridge::execute, tname+'=size('+var_name+')'
        sz=self->idl_idlbridge::getvar(tname)
        self->idl_idlbridge::execute, 'delvar, '+tname
        
        sz='['+strjoin(string(sz,format='(i0)'),',')+']'
        
        aux=execute('shmmap, '''+segname+''',size='+sz+',/destroy_segment',1,1)
        aux=execute('result=shmvar('''+segname+''')',1,1)
        
      endif else result=self->idl_idlbridge::getvar(var_name)
      
    end
  endcase
  
  return, result
end

;+
; xidl_idlbridge::getresult function method retrieves the result of the child process. if the process is not 
;   finished, the bridge will wait until the exeution is completed. all additional output parameters
;   and keywords previsouly passed to xidl_idlbridge::call_function or call_procedure can be supplied to 
;   retrieve the result
; 
; todo see cpu_process_manager::getresult
; @param  pi 
; @keyword _extra
;
; @returns the result of the called function or 1 if the executed process was a procedure or a batch file
;-
function xidl_idlbridge::getresult, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, $
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, _ref_extra=extra
    
  self->waitend
  
  ;; get result
  if keyword_set(self.exec_info.res_var) then result=self->getvar(self.exec_info.res_var) $
  else result=1L
  
  n_param=n_params()
  n_extra=n_elements(extra)
  
  ;; output parameters
  if n_param gt 0 and ptr_valid(self.exec_info.var) then begin
;   ww=where((*self.exec_info.var).input eq 0,nn_w)
    nn_w=n_elements(*self.exec_info.var)
    ww=lindgen(nn_w) ;; consider all input variables as input/output 
    for i=0, (n_param<nn_w)-1 do begin
      command='p'+string(i+1,format='(i0)')+'=self->getvar('''+(*self.exec_info.var)[ww[i]].name+''')'
      aux=execute(command,1,1)
    endfor
  endif
  
  ;; output keywords
  if n_extra gt 0 and ptr_valid(self.exec_info.key) then begin
    scope_level=0
    
    scope_trace=scope_traceback(/structure)
    ;; called from cpu_process_manager
    prev_call=scope_trace[n_elements(scope_trace)-2].routine
    if stregex(prev_call,'cpu_process_manager.*',/boolean,/fold_case) then scope_level--
    
    for i=0, n_extra-1 do begin
      ww=where(extra[i] eq (*self.exec_info.key).keyname,nn_w)
      if nn_w eq 1 then (scope_varfetch(extra[i],/ref_extra,level=scope_level))=self->getvar((*self.exec_info.key)[ww].name)
    endfor
  endif
  
  return, result
end

;+
; xidl_idlbridge::getproperty procedure method retrieves a property or group of properties for this object
;-
pro xidl_idlbridge::getproperty, verbose=verbose, wait_time=wait_time, _ref_extra=extra
  if arg_present(verbose) then verbose=self.verbose
  if arg_present(wait_time) then wait_time=self.wait_time
   
  if n_elements(extra) ne 0 then self->idl_idlbridge::getproperty, _extra=extra
end

;+
; xidl_idlbridge::setproperty procedure method sets the properties for this object
;-
pro xidl_idlbridge::setproperty, verbose=verbose, wait_time=wait_time, _ref_extra=extra

  if n_elements(verbose) ne 0 then self.verbose=verbose
  if n_elements(wait_time) ne 0 then self.wait_time=wait_time > 0.0001
  
  if n_elements(extra) ne 0 then self->idl_idlbridge::setproperty, _extra=extra
end

;+
; xidl_idlbridge::procedure method performs all cleanup when the bridge is destroyed
;   if the bridge is not available, wait for the process to finish the execution before destroying the bridge object
; todo it is inconsistent with the description written at cpu_process_manager
;-
pro xidl_idlbridge::cleanup

  ptr_free, self.seg_names
  ptr_free, [self.exec_info.var,self.exec_info.key]
  
  while ~self->isavailable() do wait, self.wait_time
  
  self->idl_idlbridge::cleanup
end

;+
; xidl_idlbridge class definition
;   
;   @inherits idl_idlbridge
;   
;   @properties objects of this class have the following properties:
;   
;   @property wait_time a floating-point value specifying the duration time, in seconds, that the xidl_idlbridge use
;     to suspend the execution before querying (in an infinite-loop) the state of the process until it is completed, 
;     aborted or finished due an error. this time could not be less than 0.0001 seconds. the default value
;     is 0.1. note that setting a small wait time may affect the performance of your application because resting in a 
;     loop querying and waiting the bridges also consumes cpu
;   @property verbose the keyword verbose causes the xidl_idlbridge to issue informative messages about the internal 
;     operations that are going on
; 
;   see idl_idlbridge properties on the idl help page to see the description of the inherit properties of the bridge
; 
;   @field verbose the flag to print informative messages
;   @field wait_time the suspending time, in seconds
;   @field exec_info a private structure used to store the result variable, output aguments and keyword names of the
;     called procedure or function
;-
pro xidl_idlbridge__define

  struct = { xidl_idlbridge, $
    inherits idl_idlbridge, $
    
    multithread: 0, $
    
    verbose: 0, $
    
    pid: 0L, $
    
    wait_time: 0., $
    seg_names: ptr_new(), $
    
    exec_info: { xidl_idlbridge_exec_info, $
                 res_var: '', $
                 var: ptr_new(), $
                 key: ptr_new() } $
    }
    
end