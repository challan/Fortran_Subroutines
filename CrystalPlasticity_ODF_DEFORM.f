!	A sample FORTRAN program. 
!	
!	Compile with: 	f77 hello.f

	  PROGRAM MAIN
	  IMPLICIT NONE
	  INTEGER nsd,rows,cols
	  PARAMETER(rows=30,cols=3)
	  DOUBLE PRECISION m_alpha(rows,cols),n_alpha(rows,cols)
	  DOUBLE PRECISION Dmat(6,6),s_alpha_t(rows)
	  DOUBLE PRECISION F_tau(3,3),FP_t(3,3),FE_t(3,3),rot(3),rotnew(3),dt
	  DOUBLE PRECISION FP_tau(3,3),FE_tau(3,3),s_alpha_tau(12),T_tau(3,3)	
	  
	  call Read_Inputs(m_alpha,n_alpha,Dmat,s_alpha_t,F_tau,FP_t,FE_t,rot,rotnew,dt,nsd)
	  call constitutive(m_alpha,n_alpha,Dmat,s_alpha_t,F_tau,FP_t,FE_t,rot,rotnew,dt,nsd,FP_tau,FE_tau,s_alpha_tau,T_tau)
	

	  END
! ██████╗ ███████╗ █████╗ ██████╗     ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗███████╗
! ██╔══██╗██╔════╝██╔══██╗██╔══██╗    ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝██╔════╝
! ██████╔╝█████╗  ███████║██║  ██║    ██║██╔██╗ ██║██████╔╝██║   ██║   ██║   ███████╗
! ██╔══██╗██╔══╝  ██╔══██║██║  ██║    ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║   ╚════██║
! ██║  ██║███████╗██║  ██║██████╔╝    ██║██║ ╚████║██║     ╚██████╔╝   ██║   ███████║
! ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═════╝     ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝   ╚══════╝                                                                               
	  SUBROUTINE Read_Inputs(m_alpha,n_alpha,Dmat,s_alpha_t,F_tau,FP_t,FE_t,rot,rotnew,dt,nsd)
	  IMPLICIT NONE
	  INTEGER i,nrow,ncol,nsd,rows,cols
	  PARAMETER(rows=30,cols=3)
	  DOUBLE PRECISION m_alpha(rows,cols),n_alpha(rows,cols)
	  DOUBLE PRECISION Dmat(6,6),s_alpha_t(rows)
	  DOUBLE PRECISION F_tau(3,3),FP_t(3,3),FE_t(3,3),rot(3),rotnew(3),dt
	  character(len=100) :: filename,header
	
		nrow=rows
		ncol=cols
		filename='Fortran_Input_Example.txt' 
		write(*,*) filename
		open  ( unit = 1, file = filename, status='old',form='formatted')

		read (1,*) header
		write(*,*) 'Reading:',header
		do i=1,nrow
			read (1,*) m_alpha(i,1:ncol)
			write(*,*) m_alpha(i,1:ncol)
		enddo
		
		read (1,*) header
		write(*,*) 'Reading:',header
		do i=1,nrow
			read(1,*) n_alpha(i,1:ncol)
			write(*,*) n_alpha(i,1:ncol)
		enddo

		read (1,*) header
		write(*,*) 'Reading:',header
		nrow=6
		ncol=6
		do i=1,nrow
			read(1,*) Dmat(i,1:ncol)
			write(*,*) Dmat(i,1:ncol)
		enddo	

		read (1,*) header
		write(*,*) 'Reading:',header
		nrow=1
		ncol=30
		read(1,*) s_alpha_t(1:ncol)
		write(*,*) s_alpha_t(1:ncol)
			
		read (1,*) header
		write(*,*) 'Reading:',header
		nrow=3
		ncol=3
		do i=1,nrow
			read(1,*) F_tau(i,1:ncol)
			write(*,*) F_tau(i,1:ncol)
		enddo		

		read (1,*) header
		write(*,*) 'Reading:',header
		do i=1,nrow
			read(1,*) FP_t(i,1:ncol)
			write(*,*) FP_t(i,1:ncol)
		enddo	
		
		read (1,*) header
		write(*,*) 'Reading:',header
		do i=1,nrow
			read(1,*) FE_t(i,1:ncol)
			write(*,*) FE_t(i,1:ncol)
		enddo	
		
		read (1,*) header
		write(*,*) 'Reading:',header	
		read(1,*) rot(1:ncol)
		write(*,*) rot(1:ncol)
		
		read (1,*) header
		write(*,*) 'Reading:',header	
		read(1,*) rotnew(1:ncol)	
		write(*,*) rotnew(1:ncol)
		
		read (1,*) header
		write(*,*) 'Reading:',header	
		read(1,*) dt		
		write(*,*) dt
		
		read (1,*) header
		write(*,*) 'Reading:',header	
		read(1,*) nsd	
		write(*,*) nsd
		
		close(1)
		END
!  ██████╗ ██████╗ ███╗   ██╗███████╗████████╗██╗████████╗██╗   ██╗████████╗██╗██╗   ██╗███████╗
! ██╔════╝██╔═══██╗████╗  ██║██╔════╝╚══██╔══╝██║╚══██╔══╝██║   ██║╚══██╔══╝██║██║   ██║██╔════╝
! ██║     ██║   ██║██╔██╗ ██║███████╗   ██║   ██║   ██║   ██║   ██║   ██║   ██║██║   ██║█████╗  
! ██║     ██║   ██║██║╚██╗██║╚════██║   ██║   ██║   ██║   ██║   ██║   ██║   ██║╚██╗ ██╔╝██╔══╝  
! ╚██████╗╚██████╔╝██║ ╚████║███████║   ██║   ██║   ██║   ╚██████╔╝   ██║   ██║ ╚████╔╝ ███████╗
!  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝   ╚═╝   ╚═╝   ╚═╝    ╚═════╝    ╚═╝   ╚═╝  ╚═══╝  ╚══════╝
!																			
	  SUBROUTINE constitutive(m_alpha,n_alpha,Dmat,s_alpha_t,F_tau,FP_t,FE_t,rot,rotnew,dt,nsd,& !!Inputs 
	 & FP_tau,FE_tau,s_alpha_tau,T_tau) !!Outputs
	  IMPLICIT NONE
	  INTEGER nsd
	  DOUBLE PRECISION m_alpha(30,3),n_alpha(30,3)
	  DOUBLE PRECISION Dmat(6,6),s_alpha_t(30)
	  DOUBLE PRECISION F_tau(3,3),FP_t(3,3),FE_t(3,3),rot(3),rotnew(3),dt
	  DOUBLE PRECISION FP_tau(3,3),FE_tau(3,3),s_alpha_tau(30),T_tau(3,3)  
	  
	  INTEGER N_slipSys, AllocateStatus, i, j, Numb_PA
	  DOUBLE PRECISION q1,q2, Sgn, FindDet
	  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE :: q,rotmat,ELASTIC_MODULI_4T,Fpn_inv, &
	 & FE_tau_trial, F_trial, CE_tau_trial, Ee_tau_trial, Identity_Mat, SCHMID_TENSOR1,Bsym, &
	 & MAT1,temp,T_star_tau_trial, h_alpha_beta_t,A_alpha_beta,symm1, symm,FP_tau_inv
	  DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE :: resolved_shear_tau_trial,b,s_beta,h0,s_s,a, &
	 & h_beta ,Ee_tau_trial_Vec, symm1_Vec, x_beta1
	  INTEGER, DIMENSION(:), ALLOCATABLE :: PA, PA_temp,INACTIVE

	  q1=1.4d0!  NON CO-PLANAR SLIP SYSTEMS
	  q2=1.0d0!  CO-PLANAR SLIP SYSTEMS n_alpha1=n_alpha2
	  
	  N_slipSys = SIZE(m_alpha,1)
	  ALLOCATE ( q(N_slipSys, N_slipSys), STAT = AllocateStatus)
	  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

	  q(1:N_slipSys,1:N_slipSys)=q1
	  DO i=1,N_slipSys
		q(i,i)=q2
	  ENDDO
	  
	  !%% Rotation Matrix (rotmat) derived from the Rodrigues vector
	  ALLOCATE (rotmat(3, 3))  
	  rotmat(1:3,1:3)=0.d0
	  CALL odfpoint(rot,rotmat)
	  write(*,*) 'Orientation Matrix'
	  do i=1,3
		write(*,*) rotmat(i,1:3)
	  enddo	  
	  
	  !% % % % % ROTATE C MATRIX ACCORDING TO THE INITIAL ORIENTATION % % % % %
	  ALLOCATE (ELASTIC_MODULI_4T(6, 6))
	  ELASTIC_MODULI_4T(1:6,1:6)=0.d0
	  CALL elasticmoduli(rotmat,Dmat,ELASTIC_MODULI_4T)
	  write(*,*) 'Elastic Moduli'	  
	  do i=1,6
		write(*,*) ELASTIC_MODULI_4T(i,1:6)
	  enddo
	  ELASTIC_MODULI_4T(1:6,4:6) = 2.d0*ELASTIC_MODULI_4T(1:6,4:6)
	  
	  ALLOCATE (Fpn_inv(3, 3))
	  ALLOCATE (FE_tau_trial(3, 3))
	  ALLOCATE (F_trial(3, 3))
	  ALLOCATE (CE_tau_trial(3, 3))
	  ALLOCATE (Ee_tau_trial(3, 3))
	  ALLOCATE (Identity_Mat(3, 3))
	  
	  Fpn_inv(1:3,1:3)=0.d0
	  FE_tau_trial(1:3,1:3)=0.d0	
	  F_trial(1:3,1:3)=0.d0	
	  CE_tau_trial(1:3,1:3)=0.d0	
	  Ee_tau_trial(1:3,1:3)=0.d0	
	  Identity_Mat(1:3,1:3)=0.d0	
	
	  CALL matinv3(FP_t,Fpn_inv)
	  FE_tau_trial=MATMUL(F_tau,Fpn_inv)	  
	  F_trial = FE_tau_trial
	  CE_tau_trial=	MATMUL(TRANSPOSE(FE_tau_trial),FE_tau_trial)
	  DO i=1,3
		Identity_Mat(i,i)=1.d0
	  ENDDO
	  Ee_tau_trial=0.5d0*(CE_tau_trial-Identity_Mat)
	  
	  !!!!!!!! STEP 1 !!!!!!!!!!!
	  
	  ALLOCATE (SCHMID_TENSOR1(3*N_slipSys, 3))
	  SCHMID_TENSOR1(1:3*N_slipSys,1:3)=0.d0
	  ALLOCATE (Bsym(3*N_slipSys, 3))
	  Bsym(1:3*N_slipSys,1:3)=0.d0
	  ALLOCATE (MAT1(3, 3))
	  MAT1(1:3,1:3)=0.d0
 	  ALLOCATE (temp(3, 3))
	  temp(1:3,1:3)=0.d0 
	  
	  DO i=1,N_slipSys
		CALL matmulti(m_alpha(i,1:3),n_alpha(i,1:3),MAT1)
		CALL ROTATION(rotmat,MAT1,temp)
		SCHMID_TENSOR1(3*(i-1)+1:3*i,1:3) = temp
		Bsym(3*(i-1)+1:3*i,1:3) = MATMUL(CE_tau_trial,temp)+MATMUL(TRANSPOSE(temp),TRANSPOSE(CE_tau_trial)) 
	  ENDDO
	
	  !!!!!!!! STEP 2 !!!!!!!!!!!
	  ALLOCATE (Ee_tau_trial_Vec(6))
	  Ee_tau_trial_Vec(1:6)=0.d0 
	  CALL vecform(Ee_tau_trial,Ee_tau_trial_Vec)
	  ALLOCATE (T_star_tau_trial(3, 3))
	  T_star_tau_trial(1:3,1:3)=0.d0 
	  CALL matform(MATMUL(ELASTIC_MODULI_4T,Ee_tau_trial_Vec),T_star_tau_trial)
	  write(*,*) 'T_star_tau_trial'
	  do i=1,3
	  write(*,*) T_star_tau_trial(i,1:3)
	  enddo
	  
	  
	  ALLOCATE (resolved_shear_tau_trial(N_slipSys))
	  ALLOCATE (b(N_slipSys))
	  resolved_shear_tau_trial(1:N_slipSys)=0.d0
	  b(1:N_slipSys)=0.d0
	  Numb_PA=0
	  DO i=1,N_slipSys
		  !!!!!!!! STEP 3 !!!!!!!!!!!
		  !Dot-Product between T_star_tau_trial and SCHMID_TENSOR1 => use * instead of MATMUL
		  resolved_shear_tau_trial(i)=SUM(T_star_tau_trial*SCHMID_TENSOR1(3*(i-1)+1:3*i,1:3))

		  !!!!!!!! STEP 4 !!!!!!!!!!!
		  b(i)=ABS(resolved_shear_tau_trial(i))-s_alpha_t(i)
		
		  IF (b(i) .ge. -300.d0) THEN
			IF (Numb_PA .eq. 0) THEN
				Numb_PA=Numb_PA+1
				ALLOCATE (PA(Numb_PA))
				PA(Numb_PA)=i
			ELSE IF (Numb_PA .gt. 0) THEN
				ALLOCATE (PA_temp(Numb_PA))
				PA_temp=PA
				DEALLOCATE (PA)
				Numb_PA=Numb_PA+1
				ALLOCATE (PA(Numb_PA))
				PA(1:Numb_PA-1)=PA_temp(1:Numb_PA)
				PA(Numb_PA)=i
				DEALLOCATE (PA_temp)
			ENDIF
		  ENDIF
	  ENDDO
	  
	  !!!!!!!! STEP 5 !!!!!!!!!!!
	  ALLOCATE (h0(N_slipSys))
	  ALLOCATE (s_s(N_slipSys))
	  ALLOCATE (a(N_slipSys))
	  !basal 3, prismatic 3, twinning 6, pyramidal a 12, pyrcplusa 6 (for HCP)
	  h0(1:N_slipSys) = 216.18d0
	  s_s(1:N_slipSys) = 900.d0
	  a(1:N_slipSys)= 0.25d0
	  ALLOCATE (h_beta(N_slipSys))
	  ALLOCATE (s_beta(N_slipSys))
	  s_beta=s_alpha_t
	  h_beta=h0*(1.d0-s_beta/s_s)**a !element-wise calculation is default in Fortran
	  ALLOCATE(h_alpha_beta_t(N_slipSys,N_slipSys))
	  h_alpha_beta_t(1:N_slipSys,1:N_slipSys)=0.d0
	  DO i=1,N_slipSys
		DO j=1,N_slipSys
			h_alpha_beta_t(i,j)=q(i,j)*h_beta(j) ! Eq(65) of Anand & Kothari
		ENDDO
	  ENDDO
	  
	  ALLOCATE(A_alpha_beta(N_slipSys,N_slipSys))
	  A_alpha_beta=h_alpha_beta_t
	  
	  
	  ALLOCATE(symm1(3,3))
	  ALLOCATE(symm(3,3))
	  ALLOCATE(symm1_Vec(6))
	  
	  symm1(1:3,1:3)=0.d0
	  symm(1:3,1:3)=0.d0
	  DO i=1,N_slipSys
		DO j=1,N_slipSys
			symm1=MATMUL(CE_tau_trial,SCHMID_TENSOR1(3*(j-1)+1:3*j,1:3))
			symm1=0.5d0*(symm1+TRANSPOSE(symm1))
			CALL vecform(symm1,symm1_Vec)
			CALL matform(MATMUL(ELASTIC_MODULI_4T,symm1_Vec),symm)
			A_alpha_beta(i,j)=A_alpha_beta(i,j)+Sgn(resolved_shear_tau_trial(i))*Sgn(resolved_shear_tau_trial(j))&
	 &						  *SUM(SCHMID_TENSOR1(3*(i-1)+1:3*i,1:3)*symm)					
		ENDDO
	  ENDDO	

	  FP_tau=FP_t
	  ALLOCATE(FP_tau_inv(3,3))
	  FP_tau_inv(1:3,1:3)=0.d0
	  CALL matinv3(FP_tau,FP_tau_inv)
	  FE_tau=MATMUL(F_tau,FP_tau_inv)
	  T_tau=MATMUL(FE_tau,FindDet(FE_tau,3)**(-1.d0)*T_star_tau_trial)
	  T_tau=MATMUL(T_tau,TRANSPOSE(FE_tau))
	  s_alpha_tau=s_alpha_t
	  
	  write(*,*) ' Number of Potentially Active Slip Systems:',Numb_PA
	  ALLOCATE(x_beta1(N_slipSys))
	  ALLOCATE(INACTIVE(N_slipSys))  
	  IF (Numb_PA .gt. 0) THEN
		CALL INACTIVE_SLIP_REMOVAL(A_alpha_beta,b,PA,N_slipSys,Numb_PA,INACTIVE,x_beta1)
		DEALLOCATE(PA)
		ALLOCATE(PA(Numb_PA))
		CALL find_active(PA,INACTIVE,N_slipSys,Numb_PA)
		write(*,*)'Final No. of Active Slip Systems:',Numb_PA
		write(*,*) 'Final PA:',PA
	  ENDIF


	  
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	  
	  SUBROUTINE odfpoint(r,OrientationMatrix)
	  IMPLICIT NONE
	  DOUBLE PRECISION r(3),rdotr,eye(3,3),OrientationMatrix(3,3),term1,term2
	  INTEGER i,j
!	  	Author: Veera
!		USAGE: [C] = odfpoint(R)
!		TO OBTAIN ORIENTATION MATRICES FROM RODRIGUES FORM	  
	  rdotr = 0.d0
	  eye(1:3,1:3)=0.d0
	  OrientationMatrix(1:3,1:3)=0.d0
	  DO i=1,3
		rdotr = rdotr+r(i)*r(i)
		eye(i,i) = 1.d0
	  ENDDO

	  term1 = 1.d0 - rdotr
	  term2 = 1.d0 + rdotr
	  
	  OrientationMatrix = eye(1:3,1:3)*term1;
	  
	  DO i=1,3
		DO j=1,3
			OrientationMatrix(i, j) = OrientationMatrix(i, j) + 2.d0*(r(i)*r(j))
		ENDDO
	  ENDDO
	  
	  OrientationMatrix(1, 2) = OrientationMatrix(1, 2)-2.d0*r(3)
	  OrientationMatrix(1, 3) =  OrientationMatrix(1, 3)+2.d0*r(2)
	  OrientationMatrix(2, 3) = OrientationMatrix(2, 3)-2.d0*r(1)
	  OrientationMatrix(2, 1) =  OrientationMatrix(2, 1)+2.d0*r(3)
	  OrientationMatrix(3, 1) = OrientationMatrix(3, 1)-2.d0*r(2)
	  OrientationMatrix(3, 2) =  OrientationMatrix(3, 2)+2.d0*r(1)
	  OrientationMatrix = (OrientationMatrix*1.d0/term2)
	  
	  RETURN
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  SUBROUTINE elasticmoduli(R,s,snew)
	  IMPLICIT NONE
	  DOUBLE PRECISION R(3, 3),s(6,6),newr(6,6),snew(6,6)
! 		%Author: Veera
! 		%USAGE: [C] = elasticmoduli(R)
! 		%The crystal elasticity matrix (6x6) is given as 's' inside the program
! 		%The function obtains the elasticity
! 		%matrix (6x6) 'snew' in sample direction
! 		%for a given crystal  with rodrigues orientation r

! 		% Program works through conversion of the rotation matrix
! 		% to a 6 by 6 form 'R' and doing snew = R*s*R'
		newr(1:6,1:6)=0.d0
		newr(1,1) = R(1,1)*R(1,1);
		newr(1,2) = R(1,2)*R(1,2);
		newr(1,3) = R(1,3)*R(1,3);
		newr(1,4) = 2.d0*R(1,2)*R(1,3);
		newr(1,5) = 2.d0*R(1,3)*R(1,1);
		newr(1,6) = 2.d0*R(1,1)*R(1,2);
		newr(2,1) = R(2,1)*R(2,1);
		newr(2,2) = R(2,2)*R(2,2);
		newr(2,3) = R(2,3)*R(2,3);
		newr(2,4) = 2.d0*R(2,2)*R(2,3);
		newr(2,5) = 2.d0*R(2,3)*R(2,1);
		newr(2,6) = 2.d0*R(2,1)*R(2,2);
		newr(3,1) = R(3,1)*R(3,1);
		newr(3,2) = R(3,2)*R(3,2);
		newr(3,3) = R(3,3)*R(3,3);
		newr(3,4) = 2.d0*R(3,2)*R(3,3);
		newr(3,5) = 2.d0*R(3,3)*R(3,1);
		newr(3,6) = 2.d0*R(3,1)*R(3,2);
		newr(4,1) = R(2,1)*R(3,1);
		newr(4,2) = R(2,2)*R(3,2);
		newr(4,3) = R(2,3)*R(3,3);
		newr(4,4) = R(2,2)*R(3,3) + R(2,3)*R(3,2);
		newr(4,5) = R(2,3)*R(3,1) + R(2,1)*R(3,3);
		newr(4,6) = R(2,1)*R(3,2) + R(2,2)*R(3,1);
		newr(5,1) = R(3,1)*R(1,1);
		newr(5,2) = R(3,2)*R(1,2);
		newr(5,3) = R(3,3)*R(1,3);
		newr(5,4) = R(1,2)*R(3,3) + R(1,3)*R(3,2);
		newr(5,5) = R(1,3)*R(3,1) + R(1,1)*R(3,3);
		newr(5,6) = R(1,1)*R(3,2) + R(1,2)*R(3,1);
		newr(6,1) = R(1,1)*R(2,1);
		newr(6,2) = R(1,2)*R(2,2);
		newr(6,3) = R(1,3)*R(2,3);
		newr(6,4) = R(1,2)*R(2,3) + R(1,3)*R(2,2);
		newr(6,5) = R(1,3)*R(2,1) + R(1,1)*R(2,3);
		newr(6,6) = R(1,1)*R(2,2) + R(1,2)*R(2,1);
	    
		snew = MATMUL(newr,s)
		snew = MATMUL(snew,TRANSPOSE(newr))
		
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  SUBROUTINE matinv3(A,B) 
    !! Performs a direct calculation of the inverse of a 3×3 matrix.
		IMPLICIT NONE
		DOUBLE PRECISION, intent(in) :: A(3,3)   !! Matrix
		DOUBLE PRECISION             :: B(3,3)   !! Inverse matrix
		DOUBLE PRECISION             :: detinv

		! Calculate the inverse determinant of the matrix
		detinv =(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2) &
	 & - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1) &
	 & + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))
		IF (detinv .eq. 0) STOP "*** Singularity Detected ***"
		detinv=1.d0/detinv
		
		! Calculate the inverse of the matrix
		B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
		B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
		B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
		B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
		B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
		B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
		B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
		B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
		B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  SUBROUTINE matmulti(m,n,mat)
	  !! Calculates matrix multiplication of m'*n, where m and n are (1,3) matrices
	  IMPLICIT NONE
	  DOUBLE PRECISION, intent(in) :: m(1,3), n(1,3) 
	  DOUBLE PRECISION mat(3,3)
	  INTEGER i,j
	  
	  DO i=1,3
		DO j=1,3
			mat(i,j)=m(1,i)*n(1,j)
		ENDDO
	  ENDDO	  
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  SUBROUTINE ROTATION(ROT,MAT,ROTATED)
	  !% Rotate Schmid Tensor from crystal coordinate to sample coordinate
	  IMPLICIT NONE
	  DOUBLE PRECISION, intent(in) :: ROT(3,3), MAT(3,3)
	  DOUBLE PRECISION ROTATED(3,3)
  
	  ROTATED = MATMUL(ROT,MAT) 
	  ROTATED = MATMUL(ROTATED,TRANSPOSE(ROT))	
 	  
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  SUBROUTINE vecform(A,Av)
	  !! convert 3by3 matrix into a 6 component column vector
	  IMPLICIT NONE
	  DOUBLE PRECISION, intent(in) :: A(3,3)
	  DOUBLE PRECISION Av(6)
	  
	  Av(1) = A(1,1)
	  Av(2) = A(2,2)
	  Av(3) = A(3,3)
	  Av(4) = A(2,3)
	  Av(5) = A(1,3)
	  Av(6) = A(1,2)	  
	   
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  SUBROUTINE matform(Av,A)
	  !! convert a 6 component column vector into a symmetric 3by3 matrix
	  IMPLICIT NONE
	  DOUBLE PRECISION, intent(in) :: Av(6)
	  DOUBLE PRECISION A(3,3)
	  
	  A(1,1) = Av(1)
	  A(1,2) = Av(6)
	  A(1,3) = Av(5)
	  A(2,1) = Av(6)
	  A(2,2) = Av(2)
	  A(2,3) = Av(4)	  
	  A(3,1) = Av(5)
	  A(3,2) = Av(4)
	  A(3,3) = Av(3)
	  
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  DOUBLE PRECISION FUNCTION Sgn(var)
	  !! Return sign of a variable (both input and output are double precision)
	  IMPLICIT NONE
	  DOUBLE PRECISION, intent(in) :: var
	  
	  IF (var .ge. 0.d0) Sgn=1.d0
	  IF (var .lt. 0.d0) Sgn=-1.d0
	  RETURN
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!		
	  DOUBLE PRECISION FUNCTION FindDet(matrix, n)
	!http://web.hku.hk/~gdli/UsefulFiles/Example-Fortran-program.html
	!Function to find the determinant of a square matrix
	!Author : Louisda16th a.k.a Ashwith J. Rego
	!Description: The subroutine is based on two key points:
	!1] A determinant is unaltered when row operations are performed: Hence, using this principle,
	!row operations (column operations would work as well) are used
	!to convert the matrix into upper traingular form
	!2]The determinant of a triangular matrix is obtained by finding the product of the diagonal elements
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(n,n) :: matrix
    INTEGER, INTENT(IN) :: n
    DOUBLE PRECISION :: m, temp
    INTEGER :: i, j, k, l
    LOGICAL :: DetExists = .TRUE.
    l = 1
    !Convert to upper triangular form
    DO k = 1, n-1
        IF (matrix(k,k) == 0) THEN
            DetExists = .FALSE.
            DO i = k+1, n
                IF (matrix(i,k) /= 0) THEN
                    DO j = 1, n
                        temp = matrix(i,j)
                        matrix(i,j)= matrix(k,j)
                        matrix(k,j) = temp
                    END DO
                    DetExists = .TRUE.
                    l=-l
                    EXIT
                ENDIF
            END DO
            IF (DetExists .EQV. .FALSE.) THEN
                FindDet = 0
                return
            END IF
        ENDIF
        DO j = k+1, n
            m = matrix(j,k)/matrix(k,k)
            DO i = k+1, n
                matrix(j,i) = matrix(j,i) - m*matrix(k,i)
            END DO
        END DO
    END DO
    
    !Calculate determinant by finding product of diagonal elements
    FindDet = l
    DO i = 1, n
        FindDet = FindDet * matrix(i,i)
    END DO
    
END FUNCTION FindDet
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  SUBROUTINE INACTIVE_SLIP_REMOVAL(A,b,PA,N_slipSys,Numb_PA,INACTIVE,x_beta1)
	  ! Iteratively removes slip systems with shear rates less than 0 among potentially active slip systems
	  IMPLICIT NONE
	  INTEGER, intent(in) :: N_slipSys, PA(Numb_PA)
	  DOUBLE PRECISION, intent(in) :: A(N_slipSys,N_slipSys)
	  DOUBLE PRECISION, intent(in) :: b(N_slipSys)

	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: b1, x_beta1, x_beta2
	  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::A1, PINV
	  INTEGER, DIMENSION(:), ALLOCATABLE :: PA_temp, INACTIVE, PA_large_temp
	  INTEGER M, N, i, Numb_PA, Numb_PA_temp, flag1, count_neg 
	  
	  !! Calculate shear increments x=A^(+)b, where A^(+) is the pseudoinverse of A
	  ALLOCATE(A1(Numb_PA,Numb_PA))  
	  ALLOCATE(PINV(Numb_PA,Numb_PA))
	  ALLOCATE(b1(Numb_PA))
	  ALLOCATE(PA_temp(Numb_PA))
	  PA_temp=PA

	  A1=A(PA_temp,PA_temp)
	  b1=b(PA_temp)
	  M=Numb_PA
	  N=Numb_PA	 	 
	  CALL pseudoinverse(A1,PINV,M,N)
	  DEALLOCATE(x_beta1) ! Clear variable from the previous subroutine
	  ALLOCATE(x_beta1(Numb_PA))	  
	  x_beta1=MATMUL(PINV,b1)

	  !Find inactive slip systems 
	  DEALLOCATE(INACTIVE)
	  ALLOCATE(INACTIVE(N_slipSys-Numb_PA))	
	  CALL find_inactive(PA_temp,INACTIVE,N_slipSys,Numb_PA) 
	  write(*,*) 'Initial ACTIVE Slip Systems:',PA_temp
	  write(*,*) 'Initial INACTIVE Slip Systems:',INACTIVE
	  
	  !Set the size of shear increments array equal to N_slipSys
	  ALLOCATE(x_beta2(Numb_PA))
	  x_beta2=x_beta1
	  DEALLOCATE(x_beta1)
	  ALLOCATE(x_beta1(N_slipSys))
	  x_beta1(1:N_slipSys)=0.d0
	  x_beta1(PA_temp)=x_beta2
	  write(*,*) 'Initial Slip Increment Rates:', x_beta1
	  
	  !only accept positive shear increments at PA slip systems
	  !remove slip systems with shear increments <= 0 by updating PA and INACTIVE
	  ALLOCATE(PA_large_temp(N_slipSys))
	  flag1=0
	  Numb_PA_temp=Numb_PA
	  WHILE (flag1 .eq. 0)
		count_neg=0 
		DO i=1,Numb_PA_temp
			IF (x_beta1(PA_temp(i)) .lt. 0.d0) THEN
				count_neg=count_neg+1 !count slip systems with negative shear strain rates
			ENDIF
		ENDDO
		IF (count_neg .ge. 1) THEN !if there exist at least one slip system with neg. shear strain rates
		  PA_large_temp(1:N_slipSys)=0
		  Numb_PA_temp=0
		  DO i=1,N_slipSys    
			IF (x_beta1(i) .gt. 0.d0) THEN
				Numb_PA_temp=Numb_PA_temp+1
				PA_large_temp(Numb_PA_temp)=i !potentially active slip systems array with size=N_slipSys
			ENDIF
		  ENDDO
		  DEALLOCATE(PA_temp)
		  ALLOCATE(PA_temp(Numb_PA_temp))
		  PA_temp=PA_large_temp(1:Numb_PA_temp) !PA_temp only contains potentially active slip systems
		
		  DEALLOCATE(INACTIVE)
		  ALLOCATE(INACTIVE(N_slipSys-Numb_PA_temp))	
		  CALL find_inactive(PA_temp,INACTIVE,N_slipSys,Numb_PA_temp)
		  write(*,*) 'updated PA:',PA_temp
		  write(*,*) 'updated INACTIVE:',INACTIVE
		  IF (Numb_PA_temp .eq. 0) THEN
			x_beta1(1:N_slipSys)=0.d0
			EXIT ! since pseudoinverse function would return error if an emptry matrix is entered
		  ENDIF
		  DEALLOCATE(A1)
		  DEALLOCATE(PINV)
		  DEALLOCATE(b1)
		  ALLOCATE(A1(Numb_PA_temp,Numb_PA_temp))  
		  ALLOCATE(PINV(Numb_PA_temp,Numb_PA_temp))
		  ALLOCATE(b1(Numb_PA_temp))
		  A1=A(PA_temp,PA_temp)
		  b1=b(PA_temp)
		  M=Numb_PA_temp
		  N=Numb_PA_temp
  
		  CALL pseudoinverse(A1,PINV,M,N)
		  DEALLOCATE(x_beta2)
		  ALLOCATE(x_beta2(Numb_PA_temp))	  
		  x_beta2=MATMUL(PINV,b1) !compute shear strain rates for the updated pot. active slip systems
		  write(*,*) 'x_beta2',x_beta2
		  DEALLOCATE(x_beta1)
		  ALLOCATE(x_beta1(N_slipSys))
		  x_beta1(1:N_slipSys)=0.d0
		  x_beta1(PA_temp)=x_beta2

		ELSE
		  flag1=1
		ENDIF
	  ENDDO
	  
	  where (x_beta1 .lt. 0.d0) x_beta1=0.d0
	  Numb_PA=Numb_PA_temp
	  write(*,*) 'Final Slip Increment Rates::', x_beta1
	  write(*,*) '------ Done Removing Inactive Slip Systems ---------'
	  
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  SUBROUTINE pseudoinverse(A1,PINV,M,N)
	  ! Author: Chal
	  ! Calculates pseudoinverse of A1 by the SVD method.
	  ! Must include LAPACK and BLAS libraries in the compiling option for this subroutine to work
	  ! For use of this subroutine, the M-by-N matrix A1 must have M>=N
	  ! In the case of this crystal plasticity code, A1 is always symmetric
	  IMPLICIT NONE
	  INTEGER, intent(in) :: M,N
	  DOUBLE PRECISION, intent(in) :: A1(M,N)
	  	 
	  integer i, LWORK, INFO, index

      double precision, dimension(M,N) :: SIGMA_plus
	  double precision, dimension(N,M) :: PINV
	  double precision, dimension(M,M) :: U
	  double precision, dimension(N,N) :: VT
	  double precision, dimension(MAX(1,5*MIN(M,N))) :: WORK
	  double precision, dimension(MIN(M,N)) :: S
	  double precision ::  CUTOFF, RNDERR
	  DATA RNDERR / 1.0E-14 /
	  
      LWORK = MAX(1,5*MIN(M,N))
! Need to compute SVD using DGESVD routine in LAPACK (which calls BLAS). 
! *  DGESVD computes the singular value decomposition (SVD) of a real
! *  M-by-N matrix A, optionally computing the left and/or right singular
! *  vectors. The SVD is written
! *
! *       A = U * SIGMA * transpose(V)
! *
! *  where SIGMA is an M-by-N matrix which is zero except for its
! *  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
! *  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
! *  are the singular values of A; they are real and non-negative, and
! *  are returned in descending order.  The first min(m,n) columns of
! *  U and V are the left and right singular vectors of A.
! *
! *  Note that the routine returns V**T, not V.
	  

	  
      call DGESVD( 'A', 'A', M, N, A1, M, S, U, M, VT, N, &
     &          WORK, LWORK, INFO )
	
	
	! PINV= V * SIGMA^+ * transpose(U)
	! where SIGMA^+ =
	! 1. 1/S if S>cutoff 
	! 2. 0   if S<=cutoff
	
	! Cutoff value used to invert singular values. 
	  CUTOFF = 10.d0*RNDERR*maxval(A1)
	  ! write(*,*) 'cutoff value is' , CUTOFF

	  write(*,*)'Singular Values from SVD:',S
	  SIGMA_plus(1:M,1:N)=0.d0
	  index=1
	  do i=1,M
		if (S(index) .gt. cutoff) then
			SIGMA_plus(i,i) = 1/S(index)
			index = index+1
		endif
	  end do	
		
	  PINV=MATMUL(TRANSPOSE(VT),SIGMA_plus)
	  PINV=MATMUL(PINV,TRANSPOSE(U))
      
	  write(*,*) 'Done Calculating Pseudo-Inverse'
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	  
	  SUBROUTINE find_inactive(PA,INACTIVE,N_slipSys,Numb_PA)
	  ! Find inactive slip systems from active set. The INACTIVE set is sorted in this subroutine
	  IMPLICIT NONE
	  INTEGER, intent(in) :: N_slipSys, Numb_PA, PA(Numb_PA)
	  INTEGER i, ACTIVE_INDEX, INACTIVE_INDEX, INACTIVE(N_slipSys-Numb_PA)
	  
	  ACTIVE_INDEX=1
	  INACTIVE_INDEX=1
	  DO i=1, N_slipSys
		IF (i .eq. PA(ACTIVE_INDEX)) THEN
			ACTIVE_INDEX=ACTIVE_INDEX+1
		ELSE
			INACTIVE(INACTIVE_INDEX)=i
			INACTIVE_INDEX=INACTIVE_INDEX+1
		ENDIF
	  ENDDO
	  
	  END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	  
	  SUBROUTINE find_active(PA,INACTIVE,N_slipSys,Numb_PA)
	  ! Find active set of slip systems from inactive set. 
	  IMPLICIT NONE
	  INTEGER, intent(in) :: N_slipSys, Numb_PA, INACTIVE(N_slipSys-Numb_PA)
	  INTEGER i, ACTIVE_INDEX, INACTIVE_INDEX , PA(Numb_PA)
	  
	  ACTIVE_INDEX=1
	  INACTIVE_INDEX=1
	  DO i=1, N_slipSys
		IF (i .eq. INACTIVE(INACTIVE_INDEX)) THEN
			INACTIVE_INDEX=INACTIVE_INDEX+1
		ELSE
			PA(ACTIVE_INDEX)=i
			ACTIVE_INDEX=ACTIVE_INDEX+1
		ENDIF
	  ENDDO
	  
	  END	  
	  