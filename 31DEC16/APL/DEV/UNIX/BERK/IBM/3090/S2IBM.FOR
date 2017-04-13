@PROCESS DIR('ibmdir:')
      subroutine s2ibm(n,src,out)                                       
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C---------------------------------------------------------------------- 
C     PURPOSE:                                                          
C        Subroutine S2IBM converts IEEE 32 bit floating point numbers   
C        into IBM 32 bit floating point numbers.  IEEE numbers with an  
C        exponent larger than can be represented by IBM floating point  
C        are set to the maximum IBM floating point number. Likewise,    
C        IEEE numbers with an exponent too small for IBM floating point 
C        are set to zero, with the sign retained. No errors are         
C        reported.                                                      
C                                                                       
C                                                                       
C     ARGUMENTS:                                                        
c     n   - length of real*4 array                                      
c     src  - array of single precision IEEE floating point numbers of    
c           length n                                                    
c     out - output array of single precision IBM floating point numbers
c           length n                                                    
C                                                                       
C        This subroutine assumes all input are normalized floating      
C        point numbers.                                                 
C                                                                       
C IBM/370 Single Precision:                                             
C        xxxx.xxxx xxxx.xxxx xxxx.xxxx xxxx.xxxx                        
C         |--exp-|-------fraction--------------|                        
C           (7)           (24)                                          
C   value = (-1)**s * 16**(e-64) * 0.f    range = 5E-79... 7E+75        
C                                                                       
C IEEE Single Precision:                                                
C        xxxx.xxxx xxxx.xxxx xxxx.xxxx xxxx.xxxx                        
C         |--exp------| |---fraction---------------|                    
C            (8)             (23)                                       
C   value = (-1)**s * 2**(e-127) * 1.f    range = 1E-38... 3E+38        
C                                                                       
C---------------------------------------------------------------------- 
C                                                                       
c  src & out treated as integer for bit manipulation                    
      implicit none                                                     
      integer*4 n,src(n),out(n)                                         
      integer*4 i,itemp                                                 
      integer*4 s,frac,expon                                            
c                                                                       
      integer*4 fmaxibm                                                 
      integer*4 z7fffffff                                               
      integer*4 z80000000                                               
      integer*4 z7f800000                                               
      integer*4 z01000000                                               
      integer*4 z0f000000                                               
      integer*4 z007fffff                                               
      integer*4 z00000003                                               
      data fmaxibm/z7fffffff/                                           
      data z7fffffff/z7fffffff/                                         
      data z80000000/z80000000/                                         
      data z7f800000/z7f800000/                                         
      data z01000000/z01000000/                                         
      data z0f000000/z0f000000/                                         
      data z007fffff/z007fffff/                                         
      data z00000003/z00000003/                                         
c                                                                       
cibmdir: ignore recrdeps                                                
      do 100 i=1,n                                                      
c                                                                       
c       special case--if source = 0, return ibm 0                       
c                                                                       
        if (iand(src(i),z7fffffff).eq.0) then                           
          out(i) = src(i)                                               
c                                                                       
        else                                                            
          s = iand(src(i),z80000000)                                    
          expon = ishft(iand(src(i),z7f800000),-23) - 127               
          frac = iand(src(i),z007fffff)                                 
c                                                                       
c         convert 23 bit fraction to 24 bit fraction                    
c                                                                       
          frac = ishft(frac,1)                                          
c                                                                       
c         restore the implied '1' which preceeded the IEEE binary point 
c                                                                       
          frac = ior(frac,z01000000)                                    
c                                                                       
c         convert scale factor from 2**exponent to 16**exponent format  
c         adjust fraction and exponent in opposite directions until     
c         exponent is a multiple of 4, then divide the exponent by 4    
c                                                                       
          if (expon.ge.0) then                                          
            itemp = iand(expon,z00000003)                               
            frac = ishft(frac,itemp)                                   
            expon = ishft(expon,-2)                                     
          else                                                          
            itemp = iand(-expon,z00000003)                              
            frac = ishft(frac,-itemp)                                    
            expon = -ishft(-expon,-2)                                   
          endif                                                         
c                                                                       
c         reduce fraction to 24 bits                                    
c                                                                       
          if (iand(frac,z0f000000).ne.0) then                           
            frac = ishft(frac,-4)                                       
            expon = expon + 1                                           
          endif                                                         
c                                                                       
c         convert exponent to excess 64                                 
c                                                                       
          expon = expon + 64                                            
          if (expon.gt.127) then                                        
            out(i) = ior(s,fmaxibm)                                     
          else if (expon.lt.0) then                                     
            out(i) = s                                                  
          else                                                          
            out(i) = ior(s,ior(ishft(expon,24),frac))                   
          end if                                                        
        endif                                                           
100   continue                                                          
      return                                                            
      end                                                               
