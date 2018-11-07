      subroutine d2ibm(n,src,out)
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
      integer*4 src(*),out(*)
      integer*4 i,x
      integer*4 s,frachi,fraclo,expon
      integer*4 itemp
      integer*4 dmaxhi
      integer*4 dmaxlo
      integer*4 z7fffffff
      integer*4 z80000000
      integer*4 z7f000000
      integer*4 z0000000f
      integer*4 z00100000
      integer*4 z000fffff
      integer*4 z00ffffff
      integer*4 z7ff00000
      integer*4 zf0000000
      integer*4 z00000003
      integer*4 z01000000
      integer*4 z0f000000
      data dmaxhi   /z7fffffff/
      data dmaxlo   /zffffffff/
      data z7fffffff/z7fffffff/
      data z80000000/z80000000/
      data z7f000000/z7f000000/
      data z0000000f/z0000000f/
      data z00100000/z00100000/
      data z000fffff/z000fffff/
      data z00ffffff/z00ffffff/
      data z7ff00000/z7ff00000/
      data zf0000000/zf0000000/
      data z00000003/z00000003/
      data z01000000/z01000000/
      data z0f000000/z0f000000/
cibmvec: ignore recrdeps
      do 100 i=1,n*2-1,2
        x = src(i)
        if (iand(x,z7fffffff).eq.0) then
          out(i) = src(i)
          out(i+1) = src(i+1)
        else
          s = iand(x,z80000000)
          expon = ishft(iand(x,z7ff00000),-20) - 1023
          frachi = iand(x,z000fffff)
          fraclo = src(i+1)
          frachi = ior(ishft(frachi,4),
     &                 ishft(iand(fraclo,zf0000000),-28))
          fraclo = ishft(fraclo,4)
          frachi = ior(frachi,z01000000)
          if (expon.ge.0) then
            itemp = iand(expon,z00000003)
            frachi = ior(ishft(frachi,itemp),
     &                   ishft(iand(fraclo,zf0000000),itemp-32))
            fraclo = ishft(fraclo,itemp)
            expon = ishft(expon,-2)            ! divide by 4
          else
            itemp = -iand(-expon,z00000003)
            fraclo = ior(ishft(fraclo,itemp),
     &                   ishft(iand(frachi,z0000000f),32+itemp))
            frachi = ishft(frachi,itemp)
            expon = -ishft(-expon,-2)          ! divide by 4
          endif
          if (iand(frachi,z0f000000).ne.0) then
            fraclo = ior(ishft(fraclo,-4),
     &               ishft(iand(frachi,z0000000f),28))
            frachi = ishft(frachi,-4)
            expon=expon + 1
          endif
          expon = expon + 64
          if (expon.gt.127) then  ! exponent overflow--set to 370 max
            out(i) = ior(s,dmaxhi)
            out(i+1) = dmaxlo
          else if (expon.le.0) then ! exponent underflow--set to zero
            out(i) = s
            out(i+1) = 0
          else ! put number in output array
            out(i) = ior(s,ior(ishft(expon,24),frachi))
            out(i+1) = fraclo
          end if
        endif
100   continue
      return
      end
