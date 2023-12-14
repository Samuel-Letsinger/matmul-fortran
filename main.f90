function matmul(a, b)

  real :: a(:,:), b(:,:)
  real, allocatable :: c(:,:)

  allocate(c(10, 10))

  c(1, 1) = a(1, 1) * b(1, 1) + a(1, 2) * b(2, 1) + a(1, 3) * b(3, 1) + a(1, 4) * b(4, 1)
  c(1, 2) = a(1, 1) * b(1, 1) + a(1, 2) * b(2, 1) + a(1, 3) * b(3, 1) + a(1, 4) * b(4, 1)

  c(2, 1) = a(1, 1) * b(1, 1) + a(1, 2) * b(2, 1) + a(1, 3) * b(3, 1) + a(1, 4) * b(4, 1)
  c(2, 2) = a(1, 1) * b(1, 1) + a(1, 2) * b(2, 1) + a(1, 3) * b(3, 1) + a(1, 4) * b(4, 1)

  c(3, 1) = a(1, 1) * b(1, 1) + a(1, 2) * b(2, 1) + a(1, 3) * b(3, 1) + a(1, 4) * b(4, 1)
  c(3, 2) = a(1, 1) * b(1, 1) + a(1, 2) * b(2, 1) + a(1, 3) * b(3, 1) + a(1, 4) * b(4, 1)

  c(4, 1) = a(1, 1) * b(1, 1) + a(1, 2) * b(2, 1) + a(1, 3) * b(3, 1) + a(1, 4) * b(4, 1)
  c(4, 2) = a(1, 1) * b(1, 1) + a(1, 2) * b(2, 1) + a(1, 3) * b(3, 1) + a(1, 4) * b(4, 1)

  deallocate(c)

end function matmul

program main

  real, allocatable :: table1(:,:), table2(:,:), results(:,:)

  allocate(table1(10, 10), table2(10, 10), results(10, 10))

  table1 = 0.0
  table2 = 0.0
  results = 0.0

  !table1 = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]  !mock tables that did NOT work smh ;;;--;;;
  !table2 = [[1,5],[2,6],[3,7],[4,8]]                         !mock tables that did not work smh ;-;

  ! Metric 1
  table1(1,1) = 1
  table1(1,2) = 2
  table1(1,3) = 3
  table1(1,4) = 4

  table1(2,1) = 5
  table1(2,2) = 6
  table1(2,3) = 7
  table1(2,4) = 8

  table1(3,1) = 9
  table1(3,2) = 10
  table1(3,3) = 11
  table1(3,4) = 12

  table1(4,1) = 13
  table1(4,2) = 14
  table1(4,3) = 15
  table1(4,4) = 16

  ! Metric 2
  table2(1,1) = 1
  table2(2,1) = 2
  table2(3,1) = 3
  table2(4,1) = 4

  table2(1,2) = 5
  table2(2,2) = 6
  table2(3,2) = 7
  table2(4,2) = 8

  results = matmul(table1, table2)

  ! results should be:
  !  30, 70,
  !  70, 174,
  !  110, 278,
  !  150, 382
  ! verified this irl and online using matrixmultiplication.xyz

  print*, 'Matrix Multiplication Results:'
  do i = 1, size(results, 1)
    do j = 1, size(results, 2)
      print*, results(i, j)
    end do
  end do

  deallocate(table1, table2, results)

end program main