      SUBROUTINE RENUMB (IVOL, CLUN, CIND, CMAX, ICNO, OCNO, IHDR, IDIR,
     *   ODIR, IRET)
C-----------------------------------------------------------------------
C! renumbers an entry in the catalog (CA) file
C# Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2008
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
C   RENUMB performs a catalog renumber operation - move header, move
C   directory info (update catlg time), rename the files.
C   Inputs:
C      IVOL   I         Disk number
C      CLUN   I         LUN of open catalog file
C      CIND   I         FTAB pointer for CLUN
C      CMAX   I         Max number entries this catalog
C      ICNO   I         Input catalog number
C      OCNO   I         Output catalog number
C   Output:
C      IHDR   I(256)    Buffer: for header
C      IDIR   I(256)    Buffer: for input directory info
C      ODIR   I(256)    Buffer: for output directory info
C      IRET   I         Error code: 0 -> did renumber as requested
C                          1 -> input slot was empty
C                          2 -> input parm error
C                          3 -> input slot was busy
C                          4 -> output slot was occupied
C                          5 -> error during misc I/O
C                          6 -> error during renaming!
C   In general, this routine is silent about its errors and success.
C   Catastrophic failure is reported.
C-----------------------------------------------------------------------
      INTEGER   IVOL, CLUN, CIND, CMAX, ICNO, OCNO, IHDR(256),
     *   IDIR(256), ODIR(256), IRET
C
      CHARACTER ONAME*48, INAME*48, XLT*2, LT*2
      INTEGER   NWPL, NLPR, NSTAT, IP, OP, HLUN, HIND, I, J, NTIME,
     *   NNAME, LN, IN, IE, IERR, IREC, OREC, HREC
      LOGICAL   T, F
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA HLUN /16/
      DATA NSTAT, NTIME, NNAME /1,2,5/
C-----------------------------------------------------------------------
C                                       Check input
      IRET = 2
      IF ((ICNO.LT.1) .OR. (ICNO.GT.CMAX)) GO TO 999
      IF (OCNO.LT.1) GO TO 999
C                                       allow file expansion
      IF (OCNO.GT.CMAX) THEN
         IREC = 2 + (CMAX - 1) / NLPR
         OREC = 2 + (OCNO - 1) / NLPR
         I = OREC - IREC
         I = MAX (4, MIN (1000, I))
         CALL ZPHFIL ('CA', IVOL, 0, 0, ONAME, J)
         CALL ZEXPND (CLUN, IVOL, ONAME, I, J)
         IF (J.GT.0) THEN
            WRITE (MSGTXT,1000) J
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IF (I.GT.0) THEN
            CMAX = CMAX + I * NLPR
            IREC = 1
            CALL ZFIO ('READ', CLUN, CIND, IREC, ODIR, IERR)
            IF (IERR.NE.0) GO TO 999
            ODIR(3) = CMAX
            CALL ZFIO ('WRIT', CLUN, CIND, IREC, ODIR, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
      IF (OCNO.GT.CMAX) GO TO 999
C                                       Prepare I/O
      IRET = 5
      NWPL = 10
      NLPR = 256 / NWPL
      IREC = 2 + (ICNO - 1) / NLPR
      OREC = 2 + (OCNO - 1) / NLPR
      HREC = 1
      IP = MOD (ICNO - 1, NLPR) * NWPL + 1
      OP = MOD (OCNO - 1, NLPR) * NWPL + 1
C                                       Read output directory
      CALL ZFIO ('READ', CLUN, CIND, OREC, ODIR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       check output for empty
      IF (ODIR(OP).LE.0) GO TO 20
         IRET = 4
         IF (ICNO.EQ.OCNO) IRET = 0
         GO TO 999
C                                       input directory
 20   IF (OREC.EQ.IREC) THEN
         CALL COPY (256, ODIR, IDIR)
      ELSE
         CALL ZFIO ('READ', CLUN, CIND, IREC, IDIR, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF ((IDIR(IP).GT.0) .AND. (IDIR(IP).LE.USELIM)) GO TO 30
         IRET = 1
         GO TO 999
 30   IF (IDIR(IP+NSTAT).EQ.0) GO TO 40
         IRET = 3
         GO TO 999
C                                       move directory info
 40   CALL COPY (NWPL, IDIR(IP), ODIR(OP))
      IDIR(IP) = -1
      IF (IREC.EQ.OREC) ODIR(IP) = -1
      CALL CATIME (1, ODIR(OP+NTIME), IHDR)
C                                       read header
      CALL ZPHFIL ('CB', IVOL, ICNO, 1, INAME, IERR)
      CALL ZOPEN (HLUN, HIND, IVOL, INAME, F, F, T, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL ZFIO ('READ', HLUN, HIND, HREC, IHDR, IERR)
      CALL ZCLOSE (HLUN, HIND, IE)
      IF (IERR.NE.0) GO TO 950
C                                       Rename main file
      IRET = 6
      CALL H2CHR (2, 19, ODIR(OP+NNAME), XLT)
      CALL ZPHFIL (XLT, IVOL, ICNO, 1, INAME, IERR)
      CALL ZPHFIL (XLT, IVOL, OCNO, 1, ONAME, IERR)
      CALL ZRENAM (IVOL, INAME, ONAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       header file
      IE = 0
      IN = 0
      CALL ZPHFIL ('CB', IVOL, ICNO, 1, INAME, IERR)
      CALL ZPHFIL ('CB', IVOL, OCNO, 1, ONAME, IERR)
      CALL ZRENAM (IVOL, INAME, ONAME, IERR)
      IF (IERR.EQ.0) GO TO 45
         IF (IERR.NE.6) GO TO 900
            CALL ZDESTR (IVOL, ONAME, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL ZRENAM (IVOL, INAME, ONAME, IERR)
            IF (IERR.NE.0) GO TO 900
C                                       extensions
 45   CALL FXHDEX (IHDR)
      DO 60 IE = 1,KIEXTN
         CALL H2CHR (2, 1, IHDR(KHEXT+IE-1), LT)
         LN = IHDR(KIVER+IE-1)
         IF ((LN.LE.0) .OR. (LT.EQ.'  ')) GO TO 60
            DO 50 IN = 1,LN
               CALL ZPHFIL (LT, IVOL, ICNO, IN, INAME, IERR)
               CALL ZPHFIL (LT, IVOL, OCNO, IN, ONAME, IERR)
               CALL ZRENAM (IVOL, INAME, ONAME, IERR)
               IF ((IERR.EQ.0) .OR. (IERR.EQ.2)) GO TO 50
C                                       new exists by error: destroy
               IF (IERR.NE.6) GO TO 900
                  CALL ZDESTR (IVOL, ONAME, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL ZRENAM (IVOL, INAME, ONAME, IERR)
                  IF (IERR.NE.0) GO TO 900
 50            CONTINUE
 60      CONTINUE
      IRET = 0
      CALL ZFIO ('WRIT', CLUN, CIND, OREC, ODIR, IERR)
      IF (IERR.NE.0) IRET = 5
      IF (IREC.NE.OREC) CALL ZFIO ('WRIT', CLUN, CIND, IREC, IDIR, IERR)
      IF (IERR.NE.0) IRET = 5
      GO TO 999
C                                       rename error: try to correct
 900  CALL ZPHFIL (XLT, IVOL, ICNO, 1, INAME, IERR)
      CALL ZPHFIL (XLT, IVOL, OCNO, 1, ONAME, IERR)
      CALL ZRENAM (IVOL, ONAME, INAME, IERR)
      IF (IERR.EQ.0) GO TO 905
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (8)
C                                       header file
 905  IF (IE.GT.0) THEN
         CALL ZPHFIL ('CB', IVOL, ICNO, 1, INAME, IERR)
         CALL ZPHFIL ('CB', IVOL, OCNO, 1, ONAME, IERR)
         CALL ZRENAM (IVOL, ONAME, INAME, IERR)
         IF (IERR.EQ.0) GO TO 910
            WRITE (MSGTXT,1905) IERR
            CALL MSGWRT (8)
C                                       extensions
 910     DO 930 I = 1,KIEXTN
            CALL H2CHR (2, 1, IHDR(KHEXT+I-1), LT)
            LN = IHDR(KIVER+I-1)
            IF ((LN.LE.0) .OR. (LT.EQ.'  ')) GO TO 930
               DO 920 J = 1,LN
                  IF ((I.EQ.IE) .AND. (J.EQ.IN)) GO TO 999
                  CALL ZPHFIL (LT, IVOL, ICNO, J, INAME, IERR)
                  CALL ZPHFIL (LT, IVOL, OCNO, J, ONAME, IERR)
                  CALL ZRENAM (IVOL, ONAME, INAME, IERR)
                  IF ((IERR.EQ.0) .OR. (IERR.EQ.2)) GO TO 920
                     WRITE (MSGTXT,1910) LT, J
                     CALL MSGWRT (7)
 920              CONTINUE
 930        CONTINUE
         END IF
      GO TO 999
C                                       header IO error
 950  WRITE (MSGTXT,1950)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' EXPANDING THE CATALOG FILE')
 1900 FORMAT ('CNO',I5,' MAIN FILE RE-RENAME FAILED - FILE LOST')
 1905 FORMAT ('CNO',I5,' HEADER FILE RE-RENAME FAILED - FILE LOST')
 1910 FORMAT ('CNO',I5,2X,A2,' VERS',I4,' FILE LOST IN RE-RENAME')
 1950 FORMAT ('I/O ERROR',I5,' TRYING TO OPEN/READ HEADER FILE')
      END
