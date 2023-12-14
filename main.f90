function matmul(a, b)

  real :: a(:,:), b(:,:)
  real, allocatable :: c(:,:)

  allocate(c(10, 10))



  deallocate(c)

end function matmul

program main

  real, allocatable :: table1(:,:), table2(:,:), results(:,:)

  allocate(table1(10, 10), table2(10, 10), results(10, 10))

  table1 = 0.0
  table2 = 0.0
  results = 0.0

  !table1 = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
  !table2 = [[1,5],[2,6],[3,7],[4,8]]

  results = matmul(table1, table2)

  ! results should be:
  !  30, 70
  !  70, 174
  !  110, 278
  !  146, 374
  ! verified this irl and online using matrixmultiplication.xyz

  print*, 'Matrix Multiplication Results:'
  do i = 1, size(results, 1)
    do j = 1, size(results, 2)
      print*, results(i, j)
    end do
  end do

  deallocate(table1, table2, results)

end program main