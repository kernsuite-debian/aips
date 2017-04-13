      SUBROUTINE INTMIO (ILUN, ACCESS, NAME, BLC, TRC, IBSIZE, CATBLK,
     *   IMSTUF, IERR)
C-----------------------------------------------------------------------
C! Open an image file for use with GETROW
C# IO-util Map
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
C   This routine will open a map file, set values in common for use with
C   close down routine DIE and set up two arrays containing all the
C   values and counters needed by reading and writing routines
C   compatible with this one.
C   Inputs:
C      ILUN    I      Logical unit number to use for the map file.
C      ACCESS  C*4    'READ' or 'WRITE' status to mark catalog.
C                     also 'HDWR' - mark write, open non-exclusive
C      IBSIZE  I      Size of IO buffer in INTEGER*2 values.
C   In/Out: (defaults filled in)
C      NAME    C*36   "Name string" in the tradition of WaWa IO.
C      BLC     R(7)   Bottom left corner of map.
C      TRC     R(7)   Top right corner of map
C   Outputs:
C      COMMON /CFILES/  Values updated so that subroutine DIE will
C                       close this file.
C      CATBLK  I(256)   Map header.
C      IMSTUF  I(37)    IO pointers and stuff that are needed by other
C                       IO routines compatible with this one. They are:
C                      1. LUN
C                      2. FTAB index
C                      3. integer (1) or real (2) flag.
C                      4. Blanked value for integers 0=no blanking.
C                      5. Catalog slot of image.
C                      6. Size of IO buffer in bytes of all things.
C                      7. Volume number of image.
C                      8. Number of dimensions in image.
C                      9. Number of values read per row of image.
C                      10-16. Number of values along all 7 axis
C                      17-30. Window in BLC TRC pairs along all 7 axis.
C                      31-36. Current position on last six axis.
C                      37 1 if read fwd -1 is bckwrd read on 2nd axis.
C      IERR    I      Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER ACCESS*4, NAME*36
      INTEGER   ILUN, IBSIZE, CATBLK(256), IMSTUF(37), IERR
      REAL      BLC(7), TRC(7)
C
      CHARACTER INAME*12, ICLASS*6, PTYPE*2
      INTEGER   IWORK(256), ISEQ, IVOL, IUSER, MAGIC, I0, I1, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA MAGIC /32000/
C-----------------------------------------------------------------------
      IF (NCFILE.GE.FILIST) THEN
         MSGTXT = 'INTMIO: FILE LIST OVERFLOWS'
         CALL MSGWRT (8)
         IERR = 9
         GO TO 999
         END IF
C                                       Open map file & get header.
      CALL WAWA2A (NAME, INAME, ICLASS, ISEQ, PTYPE, IVOL, IUSER)
      IF (IUSER.EQ.0) IUSER = NLUSER
      IF (IUSER.EQ.MAGIC) IUSER = 0
      CALL MAPOPN (ACCESS, IVOL, INAME, ICLASS, ISEQ, PTYPE,
     *   IUSER, ILUN, IMSTUF(2), IMSTUF(5), CATBLK, IWORK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL A2WAWA (INAME, ICLASS, ISEQ, PTYPE, IVOL, IUSER, NAME)
C                                       Set up info used by DIE.
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = IVOL
      FCNO(NCFILE) = IMSTUF(5)
      FRW(NCFILE) = 0
      IF (ACCESS.EQ.'WRIT') FRW(NCFILE) = 1
      IF (ACCESS.EQ.'HDWR') FRW(NCFILE) = 1
C                                       Set window defaults. Vectors
C                                       (blcx>trcx) considered ok.
      CALL VECWIN (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC)
C                                       Set values for IMSTUF
      IMSTUF(1) = ILUN
      IMSTUF(3) = 2
      IMSTUF(4) = 0
      IMSTUF(6) = IBSIZE * 2
      IMSTUF(7) = IVOL
      IMSTUF(8) = CATBLK(KIDIM)
      CALL COPY (7, CATBLK(KINAX), IMSTUF(10))
      I0 = 17
      I1 = 18
      DO 10 I = 1,7
         IMSTUF(I0) = BLC(I) + .0001
         IMSTUF(I1) = TRC(I) + .9999
         I0 = I0 + 2
         I1 = I1 + 2
 10      CONTINUE
      DO 20 I = 1,6
         IMSTUF(30+I) = BLC(I+1) + .0001
 20      CONTINUE
      IMSTUF(37) = 1
      IF (IMSTUF(19).GT.IMSTUF(20)) IMSTUF(37) = -1
C                                       Fix window for IO if this is
C                                       a vector.
      IF (IMSTUF(17).LT.IMSTUF(18)) GO TO 30
         I = IMSTUF(18)
         IMSTUF(18) = IMSTUF(17)
         IMSTUF(17) = I
 30   IMSTUF(9) = ABS(IMSTUF(18) - IMSTUF(17)) + 1
C                                       Set counters so that an MINI3
C                                       occurs on first call to IO.
      IMSTUF(31) = IMSTUF(20)
      IF (IMSTUF(21).LE.IMSTUF(22)) IMSTUF(32) = IMSTUF(32) - 1
      IF (IMSTUF(21).GT.IMSTUF(22)) IMSTUF(32) = IMSTUF(32) + 1
C
 999  RETURN
      END
