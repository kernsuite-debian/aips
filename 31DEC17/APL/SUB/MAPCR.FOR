      SUBROUTINE MAPCR (ONAMS, NAMS, HDR, ERROR)
C-----------------------------------------------------------------------
C! create and catalog an image in the WaWa package
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
C   WaWa IO system: Create and catalog a map whose catalog description
C   is defined by the namestring NAMS, and whose size is specified by
C   the KIDIM, KINAX, and KIBPX parameters in the Header.
C   Inputs:
C      ONAMS  C*36    Namestring of related "input" file - must be
C                     complete and correct; used to complete defaults
C                     in NAMS (typically the input file namestring).
C   In/Out:
C      NAMS   C*36    Namestring NAME:CLASS:TYPE:SEQ:VOL:USID of map to
C                     be created; can contain blanks, wildcards...
C      HDR    I(256)  Catalog header for map, containing enough info to
C                     define size.  The updated header is returned for
C                     real images, not SC files
C   Outputs:
C      ERROR  I       Error code: 0 => ok
C                                10 => catalog error
C                                14 => no room in catalog
C                                21 => file already exists
C                                23 => create error
C   written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      INTEGER   HDR(256), ERROR
      CHARACTER ONAMS*36, NAMS*36
C
      CHARACTER DNAMS*36, BLANK*6, DNAME*12, DCLAS*6, DPTYP*2, ONAME*12,
     *   OCLAS*6, OPTYP*2
      LOGICAL   LSC
      INTEGER   IERR, SEQ, DUM, CNO, CATER, CREATR(4), OSEQ, IV, I,
     *   DVOL, DUSE, OVOL, OUSE
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA CATER, CREATR /10, 23,21,14,10/
      DATA BLANK /'    '/
C-----------------------------------------------------------------------
C                                             Copy header
      CALL COPY (256, HDR, CATBLK)
C                                             Prepare name
      CALL PRENAM (NAMS, DNAMS, LSC)
      CALL WAWA2A (DNAMS, DNAME, DCLAS, SEQ, DPTYP, DVOL, DUSE)
      CALL WAWA2A (ONAMS, ONAME, OCLAS, OSEQ, OPTYP, OVOL, OUSE)
      IF (LSC) SEQ = 0
      IF (.NOT.LSC) CALL MAKOUT (ONAME, OCLAS, OSEQ, BLANK, DNAME,
     *   DCLAS, SEQ)
      IF (DPTYP.EQ.' ') DPTYP = OPTYP
      IF (DUSE.LT.1) DUSE = OUSE
C                                             Copy name info into HDR
      CALL CHR2H (12, DNAME, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, DCLAS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQ
      CALL CHR2H (2, DPTYP, KHPTYO, CATH(KHPTY))
      CATBLK(KIIMU) = DUSE
C                                       Do create: map
 15   ERROR = 0
      IF (LSC) GO TO 20
         CALL MCREAT (DVOL, CNO, WBUFF, IERR)
C                                             Copy header
         IF (IERR.EQ.0) CALL COPY (256, CATBLK, HDR)
         IF (IERR.EQ.0) GO TO 40
            IF (IERR.GT.4) IERR = 4
            ERROR = CREATR(IERR)
            GO TO 990
C                                       Scratch create
 20   CONTINUE
         DVOL = 1
         IF (NSCR.GE.1) DVOL = SCRVOL(NSCR)
         IF ((NSCR.LE.0) .AND. (NCFILE.GE.1)) DVOL = FVOL(NCFILE)
         DO 30 IV = 1,NVOL
            DVOL = MOD (DVOL, NVOL) + 1
            DO 25 I = 1,10
               IF (DVOL.EQ.IBAD(I)) GO TO 30
 25            CONTINUE
            CALL MCREAT (DVOL, CNO, WBUFF, IERR)
            IF (IERR.EQ.0) GO TO 40
               IF (IERR.EQ.1) GO TO 30
                  IF (IERR.GT.4) IERR = 4
                  ERROR = CREATR(IERR)
                  GO TO 990
 30         CONTINUE
         ERROR = CREATR(1)
         GO TO 990
C                                       Return actual seq #
 40   SEQ = CATBLK(KIIMS)
      CALL A2WAWA (DNAME, DCLAS, SEQ, DPTYP, DVOL, DUSE, DNAMS)
      NAMS = DNAMS
C                                       Leave catlg at rest
      CALL CATDIR ('CSTA', DVOL, CNO, DNAME, DCLAS, DUM, DPTYP,
     *   DUSE, 'CLWR', WBUFF, IERR)
      IF (IERR.EQ.0) GO TO 50
         ERROR = CATER
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 990
C                                       Put this file in CFIL common.
 50   IF (.NOT.LSC) THEN
         IF (NCFILE.LT.FILIST) THEN
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DVOL
            FCNO(NCFILE) = CNO
            FRW(NCFILE) = 2
         ELSE
            MSGTXT = 'MAPCR: FILE LIST OVERFLOWS'
            CALL MSGWRT (8)
            IERR = 9
            END IF
      ELSE
         IF (NSCR.LT.SCRLST) THEN
            NSCR = NSCR + 1
            SCRVOL(NSCR) = DVOL
            SCRCNO(NSCR) = CNO
         ELSE
            MSGTXT = 'MAPCR: SCRATCH FILE LIST OVERFLOWS'
            CALL MSGWRT (8)
            IERR = 9
            END IF
         END IF
      IF (IERR.EQ.0) GO TO 999
C
 990  CALL PRTNAM (DNAMS, 8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('CATDIR ERROR',I7,' CLEARING WRITE AFTER CREATE')
      END
