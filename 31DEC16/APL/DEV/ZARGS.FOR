        SUBROUTINE ZARGS
C-----------------------------------------------------------------------
C! sends command to/from the ARGS TV device
C# Z2 TV-IO
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
C-----------------------------------------------------------------------
C   Called by ZARGXF to write to and read from the ARGS TV device.  Has
C   entries ZARG01 through ZARG16
C   Generic version - stubs all ZARG.. entries
C-----------------------------------------------------------------------
      INTEGER   K, L, N, M, X(*), Y(*), Z(*), T(512)
      CHARACTER A*(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG01
C                                       ENTRY CLEAR
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG02 (N)
C                                       ENTRY ERASE (N)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG03 (X, Y, N)
C                                       ENTRY LINE (X,Y,N)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG04 (K, L, A)
C                                       ENTRY TEXT (K,L,A)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG05 (T)
C                                       ENTRY VIDEO (T)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG06 (N)
C                                       ENTRY COLOUR (N)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG07 (K, L, N)
C                                       ENTRY FILLA (K, L, N)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG09 (K, L, N)
C                                       ENTRY BALL (K,L,N)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG10 (N)
C                                       ENTRY LIGHTS (N)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG11 (K, L, N)
C                                       ENTRY CURSOR (K,L,N)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
C     ENTRY RETIC (K,L,N)
C-----------------------------------------------------------------------
      ENTRY ZARG12 (K, L)
C                                       ENTRY RETIC (K,L)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG13 (K, L, Z, N, M)
C                                       ENTRY LOAD (K, L, Z, N, M)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG14 (K, L, Z, N, M)
C                                       ENTRY DAOL (K,L,Z,N,M)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG15 (K, L, N)
C                                       ENTRY ZOOM (K,L,N)
C-----------------------------------------------------------------------
      GO TO 990
C-----------------------------------------------------------------------
      ENTRY ZARG16 (K, L)
C                                       ENTRY SCROLL (K,L)
C-----------------------------------------------------------------------
C
 990  MSGTXT = 'ZARGS.. REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (10)
      IF (NLUSER.GT.0) STOP 'ZARGS missing'
C
 999  RETURN
      END
