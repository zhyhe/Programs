module typedef      
        implicit none
        !$$$$$$$$$$     数据类型source,存放源的参数     $$$$$$$$$$!
        type source
                real(8) :: alpha,delta,varphi,iota,M,eta,t_c,psi_c,lndL,z
        end type source

        type detector
                real(8) :: psi,lambda,gama,zeta
        end type detector

        !$$$$$$$$$$     数据类型para,存放宇宙学参数     $$$$$$$$$$!
        type para
                real(8) :: w0,wa,Omega_m,Omega_k,h0
        end type para
end module typedef

