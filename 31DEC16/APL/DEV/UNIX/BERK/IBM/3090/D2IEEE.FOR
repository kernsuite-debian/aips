@process dir('ibmdir:')
      subroutine d2ieee(n,src,out)
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
      implicit none
      integer*4 n,src(*),out(*)
      integer*4 i,x,itemp
      integer*4 s,frachi,fraclo,expon
      integer*4 dmaxhi
      integer*4 dmaxlo
      integer*4 z7fffffff
      integer*4 z80000000
      integer*4 z7f000000
      integer*4 z0000000f
      integer*4 z00100000
      integer*4 z000fffff
      integer*4 z00ffffff
      integer*4 z00e00000
      data dmaxhi   /z7fefffff/
      data dmaxlo   /zffffffff/
      data z7fffffff/z7fffffff/
      data z80000000/z80000000/
      data z7f000000/z7f000000/
      data z0000000f/z0000000f/
      data z00100000/z00100000/
      data z000fffff/z000fffff/
      data z00ffffff/z00ffffff/
      data z00e00000/z00e00000/
      integer shift(0:7)
      data shift/0,1,2,2,3,3,3,3/
cibmdir:  ignore recrdeps
      do 100 i=1,n*2-1,2
        x = src(i)
        if (iand(x,z7fffffff).eq.0) then
          out(i) = src(i)
          out(i+1) = src(i+1)
        else
          s = iand(x,z80000000)
          expon = ishft(iand(x,z7f000000),-24) - 64
          frachi = iand(x,z00ffffff)
          fraclo = src(i+1)
          if (expon.ge.0) then
            expon = expon*4
          else
            expon = -((-expon)*4)
          endif
          itemp = shift(ishft(iand(frachi,z00e00000),-21))
          fraclo = ior(ishft(fraclo,-itemp),
     &                 ishft(frachi,32-itemp))
          frachi = ishft(frachi,-itemp)
          expon=expon - (4 - itemp)
          frachi = iand(frachi,z000fffff)
          expon = expon + 1023
          if (expon.ge.2047) then
            out(i) = ior(s,dmaxhi)
            out(i+1) = dmaxlo
          else if (expon.le.0) then
            out(i) = s
            out(i+1) = 0
          else
            out(i) = ior(s,ior(ishft(expon,20),frachi))
            out(i+1) = fraclo
          end if
        endif
100   continue
      return
      end
