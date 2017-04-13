@PROCESS DIR('ibmdir:')
      subroutine s2ieee(n,src,out)                                              
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
C        Subroutine S2IEEE converts IBM 32 bit floating point numbers           
C        into IEEE 32 bit floating point numbers.  IBM numbers with an          
C        exponent larger than can be represented by IEEE floating point         
C        are set to the maximum IEEE floating point number. Likewise,           
C        IBM numbers with an exponent too small for IEEE floating point         
C        are set to zero, with the sign retained. No errors are                 
C        reported.                                                              
C                                                                               
C                                                                               
C     ARGUMENTS:                                                                
c     n   - length of real*4 array                                              
c     src  - array of single precision IBM floating point numbers of            
c           length n                                                            
c     out - output array of single precision IEEE floating point numbers        
c           length n                                                            
C                                                                               
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
c  src & out treated as integer for bit manipulation                            
      implicit none                                                             
      integer*4 n,src(n),out(n)                                                 
      integer*4 i,itemp                                                         
      integer*4 s,frac,expon                                                    
      integer*4 fmaxieee                                                        
      integer*4 z7fffffff                                                       
      integer*4 z80000000                                                       
      integer*4 z7f000000                                                       
      integer*4 z00ffffff                                                       
      integer*4 z00f00000                                                       
      integer*4 z007fffff                                                       
      integer*4 z00800000                                                       
      data fmaxieee /z7f7fffff/                                                 
      data z7fffffff/z7fffffff/                                                 
      data z80000000/z80000000/                                                 
      data z7f000000/z7f000000/                                                 
      data z00ffffff/z00ffffff/                                                 
      data z00f00000/z00f00000/                                                 
      data z007fffff/z007fffff/                                                 
      data z00800000/z00800000/                                                 
c                                                                               
cibmdir: ignore recrdeps                                                        
      do 100 i=1,n                                                              
c                                                                               
c       special case--if source = 0, return ieee 0                              
c                                                                               
        if (iand(src(i),z7fffffff).eq.0) then                                   
          out(i) = src(i)                                                       
c                                                                               
        else                                                                    
          s = iand(src(i),z80000000)                                            
          expon = ishft(iand(src(i),z7f000000),-24) - 64                        
          frac = iand(src(i),z00ffffff)                                         
c                                                                               
c         convert 24 bit fraction to 23 bit fraction                            
c                                                                               
          frac = ishft(frac,-1)
c                                                                               
c         convert scale factor from 16**exponent to 2**exponent by              
c         multiplying the exponent by 4                                         
c                                                                               
          if (expon.ge.0) then                                                  
            expon = ishft(expon,2)                                              
          else                                                                  
            expon = -(ishft(-expon,2))                                          
          endif                                                                 
c                                                                               
c         normalize the fraction                                                
c                                                                               
          if (frac.ne.0) then
            do while (iand(frac,z00800000).eq.0)
              frac = ishft(frac,1)
              expon = expon - 1
            end do
          endif
c                                                                               
c         drop the implied 1 preceeding the binary point                        
c                                                                               
          frac = iand(frac,z007fffff)                                           
c                                                                               
c         convert exponent to excess 127                                        
c                                                                               
          expon = expon + 127                                                   
          if (expon.ge.255) then                                                
            out(i) = ior(s,fmaxieee)                                            
          else if (expon.le.0) then                                             
            out(i) = s                                                          
          else                                                                  
            out(i) = ior(s,ior(ishft(expon,23),frac))                           
          end if                                                                
        endif                                                                   
100   continue                                                                  
      return                                                                    
      end                                                                       
