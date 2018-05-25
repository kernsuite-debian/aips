      SUBROUTINE FILDES (NAMS, ASSOC, TYPE, VER, ERROR)
C-----------------------------------------------------------------------
C! destroy the specified file or associated file
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   WaWa IO system: Destroy the file specified by NAMS, TYPE, VER
C   Inputs:
C      NAMS      C*36  NAMESTRING specifying catalog block to which
C                      file is associated: NAME,CLASS,CATTYPE,SEQ,VOL,
C                      USID.  NAME,CLASS,USID ignored for scratch files.
C      ASSOC     L     File is an associated file,i.e. not catalogd
C                      ASSOC will be taken as FALSE if NAMS(8)='SCxx'
C      TYPE      C*2   Associated file type; ignored if ASSOC is false
C      VER       I     Associated file version; ignored if ASSOC is fal
C   Outputs:
C      ERROR     I      Error code:  0 => o.k.
C                          10 => catalog error
C                          11 => map too busy to destroy
C                          12 => map not found in catalog
C                          13 => extension file not in catalog
C                          25 => other destroy errors
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER NAMS*36, TYPE*2
      LOGICAL ASSOC
      INTEGER VER, ERROR
C
      CHARACTER OPER*4, DNAMS*36, STAT*4, PNAM*48, DNAME*12, DCLASS*6,
     *   DPTYPE*2, CTEMP*2
      INTEGER   SEQ, USID, NSUB, CNO, DVOL, STAR, CATER, NOEXT, EFIND,
     *   STAERR, DESER, WARN, I, IERR, J, NOFIND
      LOGICAL   LSCR, MAIN
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA EFIND, NOEXT, STAR, CATER, NOFIND, STAERR /12,13,11,10,5,4/
      DATA DESER, WARN /25, 6/
C-----------------------------------------------------------------------
      MAIN = .NOT.ASSOC
      ERROR = CATER
C                                       Prepare NAME (scratch?,pack)
      CALL PRENAM (NAMS, DNAMS, LSCR)
      MAIN = MAIN .OR. LSCR
C                                       Make Namestr intelligble
      CALL WAWA2A (DNAMS, DNAME, DCLASS, SEQ, DPTYPE, DVOL, USID)
C                                       Look for catalog block
      OPER = 'SRCH'
      CNO = 1
      CALL CATDIR ('SRCH', DVOL, CNO, DNAME, DCLASS, SEQ, DPTYPE, USID,
     *   STAT, WBUFF, IERR)
      IF (IERR.EQ.0) GO TO 100
         IF (IERR.NE.NOFIND) GO TO 20
            WRITE (MSGTXT,1000)
            ERROR = EFIND
            GO TO 990
 20      CONTINUE
            NSUB = 1
            GO TO 980
C                                       Destroy an extension file
 100  IF (MAIN) GO TO 200
C                                       Get catalog block
         OPER = 'READ'
         NSUB = 2
         CALL CATIO ('READ', DVOL, CNO, CATBLK, 'REST', WBUFF, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.WARN)) GO TO 980
C                                       Look for EXT
         CALL FXHDEX (CATBLK)
         DO 110 I = 1,KIEXTN
            CALL H2CHR (2, 1, CATH(KHEXT+I-1), CTEMP)
            IF (TYPE.EQ.CTEMP) GO TO 120
 110        CONTINUE
         GO TO 150
C                                       Look for VER
 120     J = KIVER + I - 1
         IF ((VER.LE.0) .OR. (VER.GT.CATBLK(J))) GO TO 150
         IF (VER.EQ.CATBLK(J)) CATBLK(J) = VER - 1
         IF (CATBLK(J).EQ.0) CALL CHR2H (2, '  ', 1, CATH(KHEXT+I-1))
C                                       Update cat, check status
         OPER = 'WRIT'
         CALL CATIO ('WRIT', DVOL, CNO, CATBLK, 'REST', WBUFF, IERR)
         IF (IERR.EQ.0) GO TO 130
            IF (IERR.NE.STAERR) GO TO 125
               ERROR = STAR
               WRITE (MSGTXT,1120)
               GO TO 990
 125        CONTINUE
               NSUB = 2
               GO TO 980
C                                       Do destroy
 130     CALL ZPHFIL (TYPE, DVOL, CNO, VER, PNAM, IERR)
         CALL ZDESTR (DVOL, PNAM, IERR)
         ERROR = 0
         IF (IERR.LE.1) GO TO 999
            NSUB = 7
            OPER = 'DEST'
            ERROR = DESER
            GO TO 980
C                                       Ext not found
 150     ERROR = NOEXT
         WRITE (MSGTXT,1150) TYPE, VER
         GO TO 990
C                                       Delete whole file set
 200  CONTINUE
         I = -32000
         CALL MDESTR (DVOL, CNO, CATBLK, WBUFF, I, IERR)
         ERROR = 0
         IF (IERR.EQ.0) GO TO 999
            ERROR = DESER
            IF (IERR.NE.2) GO TO 995
               ERROR = STAR
               GO TO 999
C                                       Errors
 980  WRITE (MSGTXT,1980) SUBNAM(NSUB), OPER, IERR
C
 990  CALL MSGWRT (8)
C
 995  CALL A2WAWA (DNAME, DCLASS, SEQ, DPTYPE, DVOL, USID, DNAMS)
      CALL PRTNAM (DNAMS, 8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILDES: CAN''T FIND CATALOG BLOCK ')
 1120 FORMAT ('FILDES: CAN''T DESTROY, MAIN FILE BUSY ')
 1150 FORMAT ('FILDES: CAN''T FIND EXT ',A2,' VER ',I5)
 1980 FORMAT ('FILDES: ',A6,2X,A4,' ERROR',I7)
      END
