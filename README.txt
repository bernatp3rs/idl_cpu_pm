***********************************
******* CPU_Process_Manager *******
***********************************

The CPU_Process_Manager library allows you to manage parallel computing under IDL.

The basic idea of this library is to allow the control over a set of IDL sessions in a flexible way. From the main session, the user can create the cpu_process_manager that will initialize a set of xidl_idlbridges that will control IDL slave sessions. Every session can run as a separate process and can exchange data with the main session by copying IDL variables. If your variables are defined as shared memory segments, all sessions will be able to "see" them at the same time without copying (duplicating) anything. The execution of every session is independent of the other sessions and they will all run asynchronously, allowing overlapping executions (parallel computing).

It is very important to note that there some limitations:

    The cpu_process_manager cannot be used in the IDL virtual machine.
    Object references cannot be transferred between processes.
    Only procedures, functions and batch files are allowed to be used with the library. Object methods cannot be called.
    From the main session, all data is copied in the child processes, which may affect the performance of your application if the amount of data involved is large. The use of shared memory segments may offer a more efficient way to share data.

Warning: Incorrect use of shared memory routines can corrupt or even crash your IDL application. proper use of these low level operating system features requires systems programming experience, and is not recommended for those without such experience.

The sample code below contains the complete communications skeleton for a dynamically load balanced master/slave application. Following the code is a description of the few functions necessary to write typical parallel applications using the cpu_process_manager library.

The following program add_value is the function to be executed n times and to be parallelized:

function add_value, a
  wait, .1 ; Represents some time of computation
  return, a+1
end

The following routine is the commented example:

pro exemple_cpu_pm
  array=indgen(8,8)
  ; Result
  array_1=array
  array_2=array

  ; Serial computing case
  tt=systime(/sec)
  for i=0, n_elements(array)-1 do array_1[i]=add_value(array[i])
  tt1=systime(/sec)-tt

  ; Parallel computing case
  tt=systime(/sec)
  n_cpu=8l
  ; Initialize and get the process manager
  cpu_tm=get_cpu_process_manager(n_cpu,/verbose,wait_time=0.05)

  ; Setup by setting to all sessions to the same working directory, IDL path and 
  ; compiling the same routines as the main IDL session
  cpu_tm->setup

  ; Send one unit of work to each session
  for i=0, n_cpu-1 do task_id=cpu_tm->call_function('add_value',array[i])

  ; Loop over getting new work requests until there is no more work to be done
  for i=n_cpu, n_elements(array)-1 do begin
    ; Wait until one of the processes finish
    task_id=cpu_tm->waitend()
    ; Receive result and save it
    array_2[task_id]=cpu_tm->getresult(task_id)
    ; Send a new work unit
    task_id=cpu_tm->call_function('add_value',array[i])
  endfor

  ; There is no more work to be done, so receive all the outstanding 
  ; results from the slaves
  for i=0, n_cpu-1 do begin
    task_id=cpu_tm->waitend()
    array_2[task_id]=cpu_tm->getresult(task_id)
  endfor

  tt2=systime(/sec)-tt

  ; Compare computation time, serial vs parallel
  print, 'standard time ', tt1
  print, 'cpu_pm time   ', tt2

  ; Compare the result (the difference shoule be zero)
  print, total(abs(array_1 - array_2))
end

Copyright (c) 2014 Bernat Puigdomenech Treserras (bernat.puigdomenech@gmail.com)

This program is free software: you can redistribute it and/or modify it under the terms of the gnu general public license as published by the free software foundation, either version 3 of the license, or (at your option) any later version. This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose. See the gnu general public license for more details. You should have received a copy of the gnu general public license along with this program. 
If not, see http://www.gnu.org/licenses/
