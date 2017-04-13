      SUBROUTINE MAPCOP (NAMIN, ONAME, NAMOUT, ERROR)
C-----------------------------------------------------------------------
C! Copy a map
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   WaWa IO system: Copy a map specified by NAMIN to a new map specified
C   by NAMOUT.  ONAME is a filled-in namestring representing, for
C   example, the original input map name.  It is used to provide
C   defaults for NAMOUT.
C   Inputs:
C      NAMIN   C*36  Namestring for input map (filled in)
C      ONAME   C*36  Namestring for original input image
C      NAMOUT  C*36  Namestring for output map (can include wildcards,
C                    blanks, etc.)
C   Outputs:
C      NAMIN   C*36  Changed to that actually used
C      NAMOUT  C*36  Changed to that actually used
C      ERROR   I     Error codes: 0 => ok
C                             1 => file not open
C                             2 => bad parameters
C                           3-6 => IO errors
C                            10 => catalog read/write... error
C                            12 => map not in catalog
C                            14 => no room in FILTAB or catlg
C                            21 => file already exists
C                            22 => volume unavailable
C                            23 => create error
C                            26 => open error
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER NAMIN*(*), NAMOUT*(*), ONAME*(*)
      INTEGER   ERROR
C
      CHARACTER CHTM12*12, CHTM6*6, CHTM2*2, OPER*4, STAT*4
      LOGICAL   F, WASBLK, DONCR
      INTEGER   IERR, NX, NY, NDIM, IPLANE, NPLANE, DEP(5), LUN1, DUM,
     *   LUN2, FIND1, FIND2, IDINT, RDINT, BIND1, BIND2, BLK1, BLK2,
     *   DBLK1, DBLK2, IX, IY, CATER, NSUB, BUFSZ, I, IFIL, IFIL1,
     *   IFIL2, IND1, IND2
      REAL      XMAX, XMIN, TEMP
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA F /.FALSE./
      DATA DEP /2,1,1,1,1/
      DATA CATER /10/
      DATA LUN1, LUN2 /18, 19/
C-----------------------------------------------------------------------
      DONCR = .FALSE.
C                                       Open input file
      CALL FILOPN (LUN1, NAMIN, F, DUM, DUM, ERROR)
      IF (ERROR.NE.0) GO TO 999
C                                       Set up output header
 10   NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      XMAX = CATR(KRDMX)
      XMIN = CATR(KRDMN)
C                                       If max,min not known, find them
      IF ((XMAX.NE.FBLANK) .AND. (XMIN.NE.FBLANK) .AND. (XMAX.GT.XMIN))
     *   GO TO 20
         CALL MAPMAX (LUN1, XMAX, XMIN, ERROR)
         IF (ERROR.EQ.0) GO TO 20
            WRITE (MSGTXT,1010) ERROR
            GO TO 990
C                                       Create output map
 20   CALL MAPCR (ONAME, NAMOUT, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 999
      DONCR = .TRUE.
C                                       Open output map
      CALL FILOPN (LUN2, NAMOUT, F, DUM, DUM, ERROR)
      IF (ERROR.NE.0) GO TO 999
C                                       Find pointers
      DO 30 IFIL = 6,EFIL
         IF (FILTAB(POLUN,IFIL).EQ.LUN1) IFIL1 = IFIL
         IF (FILTAB(POLUN,IFIL).EQ.LUN2) IFIL2 = IFIL
 30      CONTINUE
C                                       Set status
      NSUB = 1
      OPER = 'CSTA'
      ERROR = CATER
      STAT = 'READ'
      CALL CATDIR ('CSTA', FILTAB(POVOL,IFIL1), FILTAB(POCAT,IFIL1),
     *   CHTM12, CHTM6, DUM, CHTM2, DUM, STAT, WBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      FILTAB(POIOP,IFIL1) = 2
      STAT = 'WRIT'
      CALL CATDIR ('CSTA', FILTAB(POVOL,IFIL2), FILTAB(POCAT,IFIL2),
     *   CHTM12, CHTM6, DUM, CHTM2, DUM, STAT, WBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      FILTAB(POIOP,IFIL2) = 1
      FIND1 = FILTAB(POFIN,IFIL1)
      FIND2 = FILTAB(POFIN,IFIL2)
      BUFSZ = 2 * LREAL
      IDINT = (IFIL2 - 6) * LREAL
      RDINT = (IFIL1 - 6) * LREAL
C                                       Determine # of planes
      NDIM = CATBLK(KIDIM)
      NPLANE = 1
      DBLK1 = 0
      DBLK2 = 0
      IF (NDIM.LE.2) GO TO 50
         DO 40 I = 3,NDIM
            NPLANE = NPLANE * CATBLK(KINAX+I-1)
 40         CONTINUE
         CALL COMOFF (NDIM, CATBLK(KINAX), DEP, DBLK1, IERR)
         CALL COMOFF (NDIM, CATBLK(KINAX), DEP, DBLK2, IERR)
 50   BLK1 = 1
      BLK2 = 1
      WASBLK = .FALSE.
C                                       Loop over planes
      DO 100 IPLANE = 1,NPLANE
         NSUB = 3
         OPER = 'WRIT'
         CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, 0, RBUF(IDINT+1),
     *      BUFSZ, BLK2, ERROR)
         IF (ERROR.NE.0) GO TO 970
         OPER = 'READ'
         CALL MINIT ('READ', LUN1, FIND1, NX, NY, 0, RBUF(RDINT+1),
     *      BUFSZ, BLK1, ERROR)
         IF (ERROR.NE.0) GO TO 970
         NSUB = 4
         DO 90 IY = 1,NY
            OPER = 'READ'
            CALL MDISK ('READ', LUN1, FIND1, RBUF(RDINT+1), BIND1,
     *         ERROR)
            IF (ERROR.NE.0) GO TO 970
            OPER = 'WRIT'
            CALL MDISK ('WRIT', LUN2, FIND2, RBUF(IDINT+1), BIND2,
     *         ERROR)
            IF (ERROR.NE.0) GO TO 970
            IND1 = BIND1 + RDINT - 1
            IND2 = BIND2 + IDINT - 1
            DO 75 IX = 1,NX
               TEMP = RBUF(IND1+IX)
               IF (TEMP.EQ.FBLANK) WASBLK = .TRUE.
               RBUF(IND2+IX) = TEMP
 75            CONTINUE
 90         CONTINUE
         OPER = 'FINI'
         CALL MDISK ('FINI', LUN2, FIND2, RBUF(IDINT+1), BIND2, ERROR)
         IF (ERROR.NE.0) GO TO 970
         BLK1 = BLK1 + DBLK1
         BLK2 = BLK2 + DBLK2
 100     CONTINUE
C                                       Close files
      CALL FILCLS (LUN1)
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      CALL CATIO ('UPDT', FILTAB(POVOL,IFIL2), FILTAB(POCAT,IFIL2),
     *   CATBLK, 'CLWR', WBUFF, IERR)
      CALL ZCLOSE (LUN2, FIND2, IERR)
      FILTAB(POLUN,IFIL2) = 0
      ERROR = 0
      GO TO 999
C                                       Error
 970  IERR = ERROR
C
 980  WRITE (MSGTXT,1980) SUBNAM(NSUB), OPER, IERR
C
 990  CALL MSGWRT (8)
C                                       Destroy output file
      IF (.NOT.DONCR) GO TO 999
         CALL FILCLS (LUN2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MAPFIX: MAX,MIN ERROR ',I6)
 1980 FORMAT ('MAPFIX: ',A6,2X,A4,' ERROR',I7)
      END
