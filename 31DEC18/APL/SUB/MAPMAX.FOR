      SUBROUTINE MAPMAX (LUN, XMAX, XMIN, ERROR)
C-----------------------------------------------------------------------
C! determine extrema of image opened by FILOPN and update header
C# IO-WaWa
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
C   WaWa IO system: Determine max and min of an map opened by FILOPN
C   and update CAT block accordingly
C   Inputs:
C      LUN     I   Logical Unit No. of map
C   Outputs:
C      XMAX    R   Maximum in map
C      XMIN    R   Minimum
C      ERROR   I   Error codes: 0 => ok
C                               1 => file not open
C                               2 => input parms error
C                             3-6 => IO errors
C                               8 => data type illegal (only
C                                    floating scalars & complex ok)
C                              10 => catalog read/... error
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      INTEGER   LUN, ERROR
      REAL      XMAX, XMIN
C
      CHARACTER CHTM12*12, CHTM6*6, CHTM2*2, OPER*4
      INTEGER   NX, NY, NPLANE, IPLANE, BLK, DB, TYPCOD,
     *   FIND, NDIM, NSUB, DEP(5), BUFSZ, BIND, DUM,
     *   RDINT, I, IERR, IFIL, IND1, J, BADTYP, NOTOPN, WARN1, WARN2,
     *   CATER
      REAL      X1, X2, X3
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA DEP /2,1,1,1,1/
      DATA NOTOPN, CATER, WARN1, WARN2, BADTYP /1, 10, 6, 7, 8/
C-----------------------------------------------------------------------
C                                       Is file open?
      ERROR = NOTOPN
      DO 10 IFIL = 6,EFIL
         IF (LUN.EQ.FILTAB(POLUN,IFIL)) GO TO 20
 10      CONTINUE
      GO TO 999
C                                       Read only floating point
 20   TYPCOD = FILTAB(POBPX,IFIL)
      ERROR = BADTYP
      IF (TYPCOD.NE.2) GO TO 999
C                                       Parms from table
      FIND = FILTAB(POFIN,IFIL)
      NX = FILTAB(PONAX,IFIL)
      NY = FILTAB(PONAX+1,IFIL)
      RDINT = LREAL * (IFIL - 6)
      BUFSZ = LREAL * 2
C                                       Get cat block, mark it read
      OPER = 'READ'
      NSUB = 2
      ERROR = CATER
      CALL CATIO ('READ', FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *   CATBLK, 'READ', WBUFF, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.WARN1) .AND. (IERR.NE.WARN2))
     *   GO TO 980
C                                       Number planes, block offset
      NDIM = CATBLK(KIDIM)
      NPLANE = 1
      DB = 0
      IF (NDIM.LE.2) GO TO 50
         DO 40 I = 3,NDIM
            NPLANE = NPLANE * CATBLK(KINAX+I-1)
 40         CONTINUE
         CALL COMOFF (NDIM, CATBLK(KINAX), DEP, DB, IERR)
C                                       Init block off, max/min
 50   BLK = 1
      X2 = 1.0E20
      X1 = -X2
C                                       Read loop over planes
      DO 100 IPLANE = 1,NPLANE
         NSUB = 3
         CALL MINIT ('READ', LUN, FIND, NX, NY, 0, RBUF(RDINT+1), BUFSZ,
     *      BLK, ERROR)
         IF (ERROR.NE.0) GO TO 970
         NSUB = 4
         DO 70 I = 1,NY
            CALL MDISK ('READ', LUN, FIND, RBUF(RDINT+1), BIND, ERROR)
            IF (ERROR.NE.0) GO TO 970
            IND1 = BIND + RDINT - 1
            DO 60 J = 1,NX
               X3 = RBUF(IND1+J)
               IF (X3.EQ.INDEF) GO TO 60
                  X1 = MAX (X1, X3)
                  X2 = MIN (X2, X3)
 60            CONTINUE
 70         CONTINUE
         BLK = DB + BLK
 100     CONTINUE
C                                       Clear read, then update
C                                       cat block on disk
      CALL CATDIR ('CSTA', FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *   CHTM12, CHTM6, DUM, CHTM2, DUM, 'CLRD', WBUFF, IERR)
      CATR(KRDMX) = X1
      CATR(KRDMN) = X2
      XMAX = X1
      XMIN = X2
      NSUB = 2
      ERROR = CATER
      OPER = 'UPDT'
      CALL CATIO ('UPDT', FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *   CATBLK, 'REST', WBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      ERROR = 0
      GO TO 999
C                                       Errors
 970  IERR = ERROR
C
 980  WRITE (MSGTXT,1980) SUBNAM(NSUB), OPER, IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('MAPMAX: ',A6,2X,A4,' ERROR',I7)
      END
