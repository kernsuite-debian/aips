      SUBROUTINE MAPIO (OP, LUN, DATA, ERROR)
C-----------------------------------------------------------------------
C! reads or writes a file opened by FILOPN (WaWa IO)
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   WaWa IO system: Do i/o from a file opened using FILOPN to area DATA
C   Inputs:
C      OP            C*4     'READ' or 'WRIT'
C      LUN           I       File logical unit no.
C   Input/output:
C      DATA(*)       R       Data in or out
C   Output:
C      ERROR         I       Error code: 0 => ok
C                                1 => file not open
C                                2 => bad input parms
C                              3-6 => IO errors
C                                8 => Bad data type (ie write integers)
C                                9 => IO is complete (software
C                                     generated EOF)
C                               10 => catalog read/write error
C                               11 => Catalog status error
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      REAL      DATA(1)
      INTEGER   LUN, ERROR
C
      CHARACTER CHTM12*12, CHTM6*6, CHTM2*2, CLROP(2)*4, OPER*4
      INTEGER   BFSZ, RDINT, WIN(4), EOM, NSUB, IOP, OLDIOP, IFIL, I,
     *   POINT, TYPCOD, NOTOPN, BADOP, BADTYP, D2, STAERR, STAR, CATER,
     *   IERR, LX, LY
      LOGICAL   SSTAT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA CLROP /'CLWR', 'CLRD'/
      DATA NOTOPN, CATER, BADOP, BADTYP /1,10,2,8/
      DATA STAR, STAERR, EOM /11, 7, 9/
C-----------------------------------------------------------------------
C                                       Is file open?
      ERROR = NOTOPN
      DO 10 IFIL = 6,EFIL
         IF (FILTAB(1,IFIL).EQ.LUN) GO TO 20
 10      CONTINUE
      GO TO 999
C                                       Find OPcode
 20   ERROR = BADOP
      IOP = 0
      IF (OP.EQ.'WRIT') IOP = 1
      IF (OP.EQ.'READ') IOP = 2
      IF (IOP.LE.0) GO TO 999
C                                       What's the old IOP
 40   OLDIOP = FILTAB(POIOP,IFIL)
      SSTAT = .FALSE.
C                                       Calculate some offsets
      RDINT = LREAL * (IFIL - 6)
      TYPCOD = FILTAB(POBPX,IFIL)
C                                       Someone has changed iop
      IF (IOP.EQ.OLDIOP) GO TO 200
C                                       Clean up WRITE operation
         IF (ABS(OLDIOP).NE.1) GO TO 50
            OPER = 'FINI'
            NSUB = 4
            CALL MDISK ('FINI', LUN, FILTAB(POFIN,IFIL), RBUF(RDINT+1),
     *         POINT, ERROR)
            IF (ERROR.NE.0) GO TO 970
C                                       Reset map counters
 50      CALL COPY (6, FILTAB(POBLC+1,IFIL), FILTAB(PODEP,IFIL))
         OLDIOP = ABS(OLDIOP)
C                                       More than window has changed
C                                       Update catalog info
         IF (IOP.EQ.OLDIOP) GO TO 90
            NSUB = 1
            ERROR = CATER
            OPER = 'CSTA'
C                                       Need to clear old status
            IF (OLDIOP.EQ.3) GO TO 60
               D2 = 99
               CALL CATDIR ('CSTA', FILTAB(POVOL,IFIL), FILTAB(POCAT,
     *            IFIL), CHTM12, CHTM6, D2, CHTM2, D2, CLROP(OLDIOP),
     *            WBUFF, IERR)
               IF (IERR.NE.0) GO TO 980
C                                       Set status
 60         CALL CATDIR ('CSTA', FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *          CHTM12, CHTM6, D2, CHTM2, D2, OP, WBUFF, IERR)
            SSTAT = (IERR.EQ.0)
            IF (SSTAT) GO TO 70
               IF (IERR.NE.STAERR) GO TO 980
                  WRITE (MSGTXT,1060) OP
                  CALL MSGWRT (8)
                  ERROR = STAR
                  GO TO 990
C                                       Get Catalog block
 70         OPER = 'READ'
            NSUB = 2
            CALL CATIO ('READ', FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *         CATBLK, 'REST', WBUFF, IERR)
            IF ((IERR.GT.0) .AND. (IERR.LE.3)) GO TO 980
C                                       Set MAX,MIN='INDE' on WRITE
 80         IF (IOP.EQ.1) THEN
               CATR(KRDMX) = FBLANK
               CATR(KRDMN) = FBLANK
               CALL CATIO ('UPDT', FILTAB(POVOL,IFIL), FILTAB(POCAT,
     *            IFIL), CATBLK, 'REST', WBUFF, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
C                                       Set old IOP
 90      FILTAB(POIOP,IFIL) = IOP
C                                       Check data type
         ERROR = BADTYP
         IF (TYPCOD.NE.2) GO TO 999
C                                       Initialize I/O
 100  OPER = OP
      NSUB = 3
C                                       Find block offsets for new plane
      CALL COMOFF (FILTAB(PODIM,IFIL), FILTAB(PONAX,IFIL),
     *   FILTAB(PODEP+1,IFIL), FILTAB(POBL,IFIL), IERR)
      FILTAB(POBL,IFIL) = 1 + FILTAB(POBL,IFIL)
      BFSZ = 2 * LINT
      LX = FILTAB(PONAX,IFIL)
      LY = FILTAB(PONAX+1,IFIL)
      WIN(1) = FILTAB(POBLC,IFIL)
      WIN(2) = FILTAB(POBLC+1,IFIL)
      WIN(3) = FILTAB(POTRC,IFIL)
      WIN(4) = FILTAB(POTRC+1,IFIL)
      CALL MINIT (OP, LUN, FILTAB(POFIN,IFIL), LX, LY, WIN,
     *   RBUF(RDINT+1), BFSZ, FILTAB(POBL,IFIL), ERROR)
      IF (ERROR.NE.0) GO TO 970
C                                       Real I/O section
C                                       Update line counters
 200  IF (FILTAB(PODEP,IFIL).LE.FILTAB(POTRC+1,IFIL)) GO TO 220
         FILTAB(PODEP,IFIL) = FILTAB(POBLC+1,IFIL)
         FILTAB(PODEP+1,IFIL) = FILTAB(PODEP+1,IFIL) + 1
         DO 205 I = 1,4
            IF (FILTAB(PODEP+I,IFIL).LE.FILTAB(POTRC+I+1,IFIL))
     *         GO TO 210
            FILTAB(PODEP+I,IFIL) = FILTAB(POBLC+I+1,IFIL)
            FILTAB(PODEP+I+1,IFIL) = FILTAB(PODEP+I+1,IFIL) + 1
 205        CONTINUE
C                                       Gone past end of map, set error
         IF (FILTAB(PODEP+5,IFIL).LE.FILTAB(POTRC+6,IFIL))
     *      GO TO 210
            ERROR = EOM
            GO TO 999
C                                       Finish pending writes
 210     IF (IOP.NE.1) GO TO 100
            CALL MDISK ('FINI', LUN, FILTAB(POFIN,IFIL), RBUF(RDINT+1),
     *         POINT, ERROR)
            IF (ERROR.EQ.0) GO TO 100
               NSUB = 4
               OPER = 'FINI'
               GO TO 970
C                                       Do the IO
 220  OPER = OP
      NSUB = 4
      LX = 1 + FILTAB(POTRC,IFIL) - FILTAB(POBLC,IFIL)
      IF (TYPCOD.EQ.3) LX = 2 * LX
C                                       Reals
 240  CONTINUE
         CALL MDISK (OP, LUN, FILTAB(POFIN,IFIL), RBUF(RDINT+1),
     *      POINT, ERROR)
         IF (ERROR.NE.0) GO TO 970
C                                       For write, copy to buffer
         IF (IOP.EQ.1) CALL RCOPY (LX, DATA, RBUF(POINT+RDINT))
         IF (IOP.EQ.2) CALL RCOPY (LX, RBUF(POINT+RDINT), DATA)
C                                       Update line counter
 300  FILTAB(PODEP,IFIL) = FILTAB(PODEP,IFIL) + 1
      ERROR = 0
      GO TO 999
C                                       Errors
 970  IERR = ERROR
C
 980  WRITE (MSGTXT,1980) SUBNAM(NSUB), OPER, IERR
C
 990  CALL MSGWRT (8)
      WRITE (MSGTXT,1990) FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL)
      CALL MSGWRT (8)
      IF (SSTAT) CALL CATDIR ('CSTA', FILTAB(POVOL,IFIL),
     *   FILTAB(POCAT,IFIL), CHTM12, CHTM6, D2, CHTM2, D2, CLROP(IOP),
     *   WBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('MAPIO: CAN''T SET STATUS TO ',A4)
 1980 FORMAT ('MAPIO: ',A6,1X,A4,' ERROR ',I6)
 1990 FORMAT ('MAPIO: ON VOLUME',I3,' CATALOG SLOT',I5)
      END
