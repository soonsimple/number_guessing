! **********************************************************************
!
! Number Guessing Game
!
! Originally written by Walt Koetke in Focal, and ported to BASIC
! by David H. Ahl. Converted to FORTRAN by Philipp Engel.
!
! Here, soonsimple converted it to Modern Fortran free style.
! 2022-4-24
!
! **********************************************************************
!
! The program requires three procedures that are not part of the ANSI/ISO FORTRAN 77 language standard:
! RESULT = TIME()
! Returns timestamp in seconds (INTEGER). Required for the initialisation of the pseudo-random number generator.
! RESULT = RAND(I)
! Returns the next random number (REAL).
! CALL SRAND(SEED)
! Initialises the pseudo-random number generator with given seed value (INTEGER).
!
! ***********************************************************************
program main
   implicit none
   integer :: input, yourtry, yourguess, imax, isecre, limit

   call srand(time())

   print *, 'This is a number guessing game.'
   print *, 'I will think of a NUMBER between 1 and any limit you want.'
   print *, 'Then you have to guess what it is.'

   print '(a,$)', ' What limit do you want? '
   limit = input()
   !read *, limit
   imax = int(log(real(limit))/log(2.0)) + 1

   print '(a,i5,a)', ' I am thinking of a number between 1 and', limit, '...'
   print '(a,$)', ' Now you try to guess what it is: '
   isecre = int(rand(0)*limit) + 1
   yourguess = 0
   yourtry = 0

   do while (.true.)
      yourtry = yourtry + 1
      yourguess = input()

      if (yourguess == isecre) Then
         exit
      else
         if (yourguess < isecre) Then
            print '(a,$)', ' Too low. try a bigger answer: '
         else if (yourguess > isecre) Then
            print '(a,$)', ' Too high. Try a smaller answer: '
         end if
      end if
   end do

   print '(a, i3, a)', ' That''s it! You got it in', yourtry, ' tries.'

   if (yourtry < imax) Then
      print *, 'Very Good!'
   else if (yourtry == imax) Then
      print *, 'Good!'
   else
      print '(a, i3,a)', ' You should have been able to get it in', imax, ' times.'
   end if
   print *, ''
end program main

function input()
   !
   ! returns a positive integer greater than 1 from user input.
   !
   integer :: input
   integer :: istat

   do while (.true.)
      read (*, '(i5)', iostat=istat) input
      if ((istat == 0) .and. (input > 1)) Then
         return
      else
         print *, 'Invalid input. Try again please.'
      end if
   end do
end function input
