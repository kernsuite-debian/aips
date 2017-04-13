      SUBROUTINE ANSEL (DISKIN, CNOIN, DISKOU, CNOOUT, IVER, OVER,
     *   CATUV, CATBLK, LUN1, LUN2, BIF, EIF, FQOFF, DOPOL, BUFF1,
     *   BUFF2, IRET)
C-----------------------------------------------------------------------
C! copies AN table adjusting ref freq and number of IFs
C# subroutine editing UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2009
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
C   Updates tables for selection by IF etc
C   Inputs:
C      DISKIN   I        Input disk number
C      CNOIN    I        Input catalog number
C      DISKOU   I        Output disk number
C      CNOOUT   I        Output catalog number
C      IVER     I        input version number to copy
C      OVER     I        output version number to write
C      CATUV    I(256)   input header
C      LUN1     I        LUN to use
C      LUN2     I        LUN to use
C      BIF      I        Beginning IF
C      EIF      I        Ending IF
C      FQOFF    D        Frequency offset to apply to header FREQ
C      DOPOL    I        > 0 => pol cal applied, clear values
C   In/out
C      CATBLK   I(256)   Output uv header (AN files added)
C      BUFF1    I(512)   Scratch
C      BUFF2    I(512)   Scratch
C   Output:
C      IRET     I        Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, DISKOU, CNOOUT, IVER, OVER, CATUV(*),
     *   CATBLK(*), LUN1, LUN2, BIF, EIF, dopol, BUFF1(*), BUFF2(*),
     *   IRET
      DOUBLE PRECISION FQOFF
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, NANT, ANOPEN, JI, JO, MSGSAV, ANKOL2(MAXANC),
     *   ANNUM2(MAXANC), KNUM, MLOC, KTYP, IERR
      HOLLERITH HSOLT(2)
      LOGICAL   REFRMT
      CHARACTER BLANK*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       open input
      ANOPEN = 0
      CALL ANTINI ('READ', BUFF1, DISKIN, CNOIN, IVER, CATUV, LUN1,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT'
         GO TO 990
         END IF
      ANOPEN = 1
      REFRMT = (FQOFF.NE.0.0D0) .OR. (ANTNIF.NE.EIF-BIF+1)
C                                       adjust and open output
      ANTNIF = EIF - BIF + 1
      SAFREQ = SAFREQ + FQOFF
      CALL ANTINI ('WRIT', BUFF2, DISKOU, CNOOUT, OVER, CATBLK, LUN2,
     *   IANRNO, ANKOL2, ANNUM2, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT'
         GO TO 990
         END IF
      ANOPEN = 2
C                                       copy
      NANT = BUFF1(5)
      DO 20 I = 1,NANT
         IANRNO = I
         CALL TABAN ('READ', BUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ INPUT'
            GO TO 990
            END IF
         JO = 1
         JI = 2 * (BIF-1) + 1
         IF (REFRMT) THEN
            DO 10 J = 1,ANTNIF
               FWHMAN(J) = FWHMAN(J+BIF-1)
               POLCA(JO)   = POLCA(JI)
               POLCA(JO+1) = POLCA(JI+1)
               POLCB(JO)   = POLCB(JI)
               POLCB(JO+1) = POLCB(JI+1)
               JI = JI + 2
               JO = JO + 2
 10            CONTINUE
            END IF
C                                       clear if applied
         IF (DOPOL.GT.0) THEN
            CALL RFILL (2*ANTNIF, 0.0, POLCA)
            CALL RFILL (2*ANTNIF, 0.0, POLCB)
            POLAA = 0.0
            POLAB = 0.0
            END IF
         IANRNO = I
         CALL TABAN ('WRIT', BUFF2, IANRNO, ANKOL2, ANNUM2, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT'
            GO TO 990
            END IF
 20      CONTINUE
C                                       Check polzn solution type
      KNUM = 1
      MLOC = 1
      KTYP = 3
      MSGSUP = 32000
      CALL TABKEY ('READ', 'POLTYPE ', KNUM, BUFF1, MLOC, HSOLT, KTYP,
     *   IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
         IF (DOPOL.GT.0) THEN
            BLANK = ' '
            CALL CHR2H (8, BLANK, 1, HSOLT)
            END IF
         CALL TABKEY ('WRIT', 'POLTYPE ', KNUM, BUFF2, MLOC, HSOLT,
     *      KTYP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFRMT) THEN
            WRITE (MSGTXT,1020) 'Reformatted AN', DISKIN, CNOIN, IVER,
     *         DISKOU, CNOOUT, OVER
         ELSE
            WRITE (MSGTXT,1020) 'Copied AN', DISKIN, CNOIN, IVER,
     *         DISKOU, CNOOUT, OVER
            END IF
         CALL MSGWRT (3)
         END IF

C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
      IF (ANOPEN.GT.0) CALL TABAN ('CLOS', BUFF1, IANRNO, ANKOLS,
     *   ANNUMV, ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *   FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, I)
      IF (ANOPEN.GT.1) CALL TABAN ('CLOS', BUFF2, IANRNO, ANKOL2,
     *   ANNUM2, ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *   FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANSEL ERROR:',I5,' ON ',A,' AN TABLE')
 1020 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
       END
