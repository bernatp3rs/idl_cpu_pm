idl_cpu_pm
==========

IDL CPU Process Manager

CPU_Process_Manager allows  you to mange parallel computing under idl. 

The basic idea of this library is to allow the control over a set of idl sessions in a flexible way. 
From the main session, the user can create the cpu_process_manager that will initialize a set of xidl_idlbridges that will control idl slave sessions. every session can run as a separate process and can exchange data with the main session by copying idl variables. if your variables are defined as shared memory segments, all sessions will be able to "see" the data at the same time without copying (duplicating) data. 
The execution of every session is independent of the other sessions and they will all run asynchronously, allowing overlapping executions (parallel computing).

It is very important to note that there some limitations:
 - the cpu_process_manager cannot be used in idl virtual machine. 
 - object references cannot be transferred between processes. 
 - only procedures, functions and batch files are allowed to use with the library. object methods cannot be called.
 - from the main session, all data is copied in the child processes, which may affect the performance of your application if the amount of data involved is large. the use of shared memory segments may offer a more efficient way to share data. 

Warning: incorrect use of shared memory routines can corrupt or even crash your idl application. proper use of these low level operating system features requires systems programming experience, and is not recommended for those without such experience. 

Code example
==========

The sample code below contains the complete communications skeleton for a dynamically load balanced master/slave
application. following the code is a description of the few functions necessary to write typical parallel applications using the cpu_process_manager library. 

The following program add_value is the function to be executed n times and to be parallelized:
  
function add_value, a
  wait, .1 ; represents some time of computation
  return, a+1
end

The following routine is the comented example:
   
pro exemple_cpu_pm
  array=indgen(8,8)
  ; result
  array_1=array
  array_2=array
  
  ; serial computing case
  tt=systime(/sec)
  for i=0, n_elements(array)-1 do array_1[i]=add_value(array[i])
    tt1=systime(/sec)-tt
 
     ; parallel computing case
     tt=systime(/sec)
     n_cpu=8l
     ; initialize and get the process manager
     cpu_tm=get_cpu_process_manager(n_cpu,/verbose,wait_time=0.05)

     ; setup by setting to all sessions the same working directory, idl path and compiling the same routines as the
     ; main idl session     
     cpu_tm->setup
  
     ; send one unit of work to each session
     for i=0, n_cpu-1 do task_id=cpu_tm->call_function('add_value',array[i])

     ; loop over getting new work requests until there is no more work to be done
     for i=n_cpu, n_elements(array)-1 do begin
       ; wait until one of the processes finish
       task_id=cpu_tm->waitend()
       ; receive result and save it
       array_2[task_id]=cpu_tm->getresult(task_id)
       ; send a new work unit
       task_id=cpu_tm->call_function('add_value',array[i])
     endfor
  
     ; there is no more work to be done, so receive all the outstanding results from the slaves
     for i=0, n_cpu-1 do begin
       task_id=cpu_tm->waitend()
       array_2[task_id]=cpu_tm->getresult(task_id)
     endfor
  
     tt2=systime(/sec)-tt
     
     ; compare computation time, serial vs parallel
     print, 'standard time ', tt1
     print, 'cpu_tm time   ', tt2
  
     ; compare the result (the difference shoule be zero)
     print, total(abs(array_1 - array_2))
  end 
