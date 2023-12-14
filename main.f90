function matmul(a, b)

  real, intent(in) :: a(:,:), b(:,:)
  real, allocatable :: c(:,:)

  integer :: i, j

  allocate(c(size(a, 1), size(b, 2)))
  print*, size(a, 1), size(b, 2)

  do i = 1, size(a, 1)
    do j = 1, size(b, 2)
      c(i, j) = dot_product(a(i, :), b(:, j))
      print*, dot_product(a(i, :), b(:, j))
    end do
  end do

  deallocate(c)

end function matmul

program main

  real, allocatable :: table1(:,:), table2(:,:), results(:,:)
  integer :: i, j, size1, size2, size3

  write(*,*) 'Enter the size for matrix 1 rows:'
  read(*,*) size1
  write(*,*) 'Enter the size for matrix 1 columns:'
  read(*,*) size2
  write(*,*) 'Enter the size for matrix 2 columns:'
  read(*,*) size3

  allocate(table1(size1, size2), table2(size2, size3), results(size1, size3))

  table1 = 0.0
  table2 = 0.0
  results = 0.0

  write(*,*) 'Enter values for Matrix 1 (all unneeded values can be left as 0):'
  do i = 1, size1
    do j = 1, size2
      write(*,*) 'Matrix1(', i, ',', j, '):'
      read(*,*) table1(i, j)
    end do
  end do

  ! Input values for Matrix 2 from the user
  write(*,*) 'Enter values for Matrix 2 (all unneeded values can be left as 0):'
  do i = 1, size2
    do j = 1, size3
      write(*,*) 'Matrix2(', i, ',', j, '):'
      read(*,*) table2(i, j)
    end do
  end do

  results = matmul(table1, table2)

  print*, 'Matrix Multiplication Results:'
  do i = 1, size(results, 1)
    print*, "["
    do j = 1, size(results, 2)
      print*, results(i, j)
    end do
    print*, "]"
  end do

  deallocate(table1, table2, results)

end program main