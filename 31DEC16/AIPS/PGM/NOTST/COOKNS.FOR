      PROGRAM COOKNS
C-----------------------------------------------------------------------
C! Converts COOKNS.SRT, the semi-sorted CookBook index, to TeX
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2004, 2012-2013
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
C   Program to read in the semi-sorted list of index entries for the
C   CookBook (produced by "sort -f COOK*.ndx > COOKNS.SRT"
C   Reads COOKNS.SRT for a list of index entries and writes two
C   different tex files for different formats of index.
C      A logical or enviroment variable named MYDIR must be defined
C   before running COOKNS.
C-----------------------------------------------------------------------
      INTEGER   NEQ
      PARAMETER (NEQ = 200)
C
      CHARACTER INLINE*132, LNLINE*132, OUTLIN*512, OUTFIL*48, LFIL*48,
     *   LTOPIC*128, ITOPIC*128, CCH(NEQ)*2, CTTYP*1, CPTYP(NEQ)*1,
     *   LFC*1, IFC*1, ITUPIC*128, LTUPIC*128, CSYM(NEQ)*20
      INTEGER   IOERR, I, J, BUFFER(512), LUNO, FINDO, LUNI, FINDI,
     *   IPT, IPS, IPTT, IPCT, IL, IP, IEQ, J1, LUNT, FINDT, IPC, IPP,
     *   JT, JTRIM
      LOGICAL   WASEOF
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNO, LUNI, LUNT /16, 17, 18/
C-----------------------------------------------------------------------
C                                       AIPS system start up stuff
      TSKNAM ='COOKNS'
      NPOPS = 1
      CALL ZDCHIN (.FALSE., BUFFER)
      DEVTAB(LUNO) = 3
      DEVTAB(LUNI) = 3
      DEVTAB(LUNT) = 3
C                                       Get list of index entries
      LFIL = 'MYDIR:COOKNS.SRT'
      CALL ZTXOPN ('READ', LUNI, FINDI, LFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       Open output file
      OUTFIL = 'MYDIR:COOKJ.TEX'
      CALL ZTXOPN ('WRIT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       Open template file
      LFIL = 'MYDIR:COOKJ.TEMP'
      CALL ZTXOPN ('READ', LUNT, FINDT, LFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       Read template until insert point
 10      CALL ZTXIO ('READ', LUNT, FINDT, INLINE, IOERR)
         IF (IOERR.NE.0) GO TO 900
         JT = JTRIM (INLINE)
         IF (INLINE.NE.'%%Insert here') THEN
            OUTLIN = INLINE
            IP = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:IP), IOERR)
            IF (IOERR.NE.0) GO TO 900
            GO TO 10
            END IF
C                                       init pointers
      LTOPIC = ' '
      LTUPIC = ' '
      WASEOF = .FALSE.
      IEQ = 0
C                                       Begin looping over input file
 15   CALL ZTXIO ('READ', LUNI, FINDI, INLINE, IOERR)
         IF (IOERR.EQ.2) THEN
            WASEOF = .TRUE.
            ITOPIC = ' '
            ITUPIC = ' '
         ELSE IF (IOERR.NE.0) THEN
            GO TO 900
         ELSE
            JT = JTRIM (INLINE)
C                                       skip blanks, repeats, comments
            IF ((INLINE.EQ.' ') .OR. (INLINE.EQ.LNLINE)) GO TO 15
            IF (INLINE(1:1).EQ.'%') GO TO 15
C                                       current pointers
            IPT = INDEX (INLINE(1:), ':') + 1
            IPC = INDEX (INLINE(IPT:), ':') + IPT
            IPP = INDEX (INLINE(IPC:), ':') + IPC
            IPS = INDEX (INLINE(IPP:), ':') + IPP
            IPTT = INDEX (INLINE(IPS:), ':') + IPS
            IPCT = INDEX (INLINE(IPTT:), ':') + IPTT
            ITOPIC = INLINE(IPT:IPC-2)
C                                       special cases
            IF (INLINE(IPTT:IPTT).NE.'T') THEN
               IF (ITOPIC.EQ.'Aips') ITOPIC = '\\AIPS'
               IF (ITOPIC.EQ.'AIPS') ITOPIC = '\\AIPS'
               IF ((ITOPIC(:5).EQ.'Pops ') .OR. (ITOPIC(:5).EQ.'POPS'))
     *            THEN
                  ITUPIC = ITOPIC
                  IF (ITUPIC(5:).EQ.' ') THEN
                     ITOPIC = '\\POPS'
                  ELSE
                     ITOPIC = '\\POPS\\' // ITUPIC(5:)
                     END IF
                  END IF
               IF (ITOPIC.EQ.'Aipsletter') ITOPIC = '\\Aipsletter'
               IF (ITOPIC.EQ.'COOKBOOK') ITOPIC = '\\COOKBOOK'
               IF (ITOPIC.EQ.'Cookbook') ITOPIC = '\\COOKBOOK'
               IF (ITOPIC.EQ.'TEX') ITOPIC = '\\TEX'
               IF (ITOPIC.EQ.'AIPS++') ITOPIC = '\\AIPTOO'
               IF (ITOPIC.EQ.'aips++') ITOPIC = '\\AIPTOO'
               IF (ITOPIC.EQ.'Going AIPS') ITOPIC = '\\it Going \\AIPS'
               END IF
            ITUPIC = ITOPIC
            CALL CHLTOU (128, ITUPIC)
            END IF
C                                       dump current list
         IF (ITUPIC.NE.LTUPIC) THEN
            IL = JTRIM (LTOPIC)
            J = 0
            OUTLIN = ' '
 20         J = J + 1
            IF (J.LE.IEQ) THEN
               OUTLIN = '\\scls{'
               IF (J.NE.1) OUTLIN(5:5) = 'p'
               IP = 7
               IF (CTTYP.EQ.'T') THEN
                  OUTLIN(IP:) = '\\tt '
                  IP = IP + 4
               ELSE IF (CTTYP.EQ.'I') THEN
                  OUTLIN(IP:) = '\\it '
                  IP = IP + 4
                  END IF
               I = JTRIM (CCH(J))
               OUTLIN(IP:) = LTOPIC(:IL) // '}{' // CCH(J)(:I) // '}{'
               IP = IP + IL + I + 4
C                                       any more in this chapter
               DO 25 I = J,MIN(J+5,IEQ)
                  IF (CCH(J).NE.CCH(I)) GO TO 30
                  J1 = I
 25               CONTINUE
 30            IF (J.LE.J1) THEN
                  I = JTRIM (CSYM(J))
                  IF (CPTYP(J).EQ.'B') THEN
                     OUTLIN(IP:) = '{\\bf \\ISec{' // CSYM(J)(:I)
     *                   // '}}'
                     IP = IP + 13 + I
                  ELSE
                     OUTLIN(IP:) = '\\ISec{' // CSYM(J)(:I) // '}'
                     IP = IP + 7 + I
                     END IF
                  IF (J.LT.J1) THEN
                     J = J + 1
                     OUTLIN(IP:) = ', '
                     IP = IP + 2
                     GO TO 30
                     END IF
                  END IF
               IP = JTRIM (OUTLIN)
               IF (IP.GT.0) THEN
                  IP = IP + 1
                  OUTLIN(IP:IP) = '}'
                  CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:IP), IOERR)
                  IF (IOERR.NE.0) GO TO 900
                  END IF
               GO TO 20
               END IF
            IEQ = 0
C                                       alphabet
            IF (.NOT.WASEOF) THEN
               CALL FNDFST (ITUPIC, IFC)
               CALL FNDFST (LTUPIC, LFC)
               IF (IFC.NE.LFC) THEN
                  OUTLIN = '\\alphasect{' // IFC // '}'
                  IP = JTRIM (OUTLIN)
                  CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:IP), IOERR)
                  IF (IOERR.NE.0) GO TO 900
                  END IF
               END IF
            END IF
C                                       add to list
         IF (.NOT.WASEOF) THEN
            IEQ = IEQ + 1
            CSYM(IEQ) = INLINE(IPS:IPTT-2)
            IP = INDEX (CSYM(IEQ), '.')
            CCH(IEQ) = CSYM(IEQ)(:IP-1)
            CPTYP(IEQ) = INLINE(IPCT:IPCT)
            IF ((IEQ.EQ.1) .OR. (CTTYP.EQ.'R')) CTTYP =
     *         INLINE(IPTT:IPTT)
            IF (LTUPIC.NE.ITUPIC) THEN
               LTOPIC = ITOPIC
               LTUPIC = ITUPIC
               END IF
            LNLINE = INLINE
            GO TO 15
            END IF
C                                       copy last of template
 50      CALL ZTXIO ('READ', LUNT, FINDT, INLINE, IOERR)
         IF (IOERR.EQ.2) THEN
            IOERR = 0
            GO TO 900
         ELSE IF (IOERR.NE.0) THEN
            GO TO 900
         ELSE
            OUTLIN = INLINE
            IP = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:IP), IOERR)
            IF (IOERR.NE.0) GO TO 900
            GO TO 50
            END IF
C                                       Done - close files
 900  IF (IOERR.NE.0) THEN
         WRITE (MSGTXT,1900) IOERR
         CALL MSGWRT (7)
         END IF
      CALL ZTXCLS (LUNO, FINDO, IOERR)
      CALL ZTXCLS (LUNI, FINDI, IOERR)
      CALL ZTXCLS (LUNT, FINDT, IOERR)
C
 999  STOP
C-----------------------------------------------------------------------
 1900 FORMAT ('ERROR',I7,' DOING READS OR WRITES')
      END
      SUBROUTINE FNDFST (TOPIC, FCH)
C-----------------------------------------------------------------------
C   finds first character by alphabetic rules
C   Input:
C      TOPIC   C*(*)   String
C   Output
C      FCH     C*1     First usable character in upper case
C-----------------------------------------------------------------------
      CHARACTER TOPIC*(*), FCH*1
C
      INTEGER   I, J, L, JTRIM
      CHARACTER LC*1, ALPHA*36, LTOPIC*132
      LOGICAL   WASBKS
      DATA ALPHA /'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'/
C-----------------------------------------------------------------------
      FCH = ' '
      L = JTRIM (TOPIC)
      L = MIN (132, L)
      LTOPIC = TOPIC(:L)
      WASBKS = .FALSE.
      IF (L.GT.0) THEN
         CALL CHLTOU (L, LTOPIC)
         DO 20 I = 1,L
            LC = LTOPIC(I:I)
            IF (WASBKS) THEN
               WASBKS = LC.NE.' '
            ELSE IF (LC.EQ.'\\') THEN
               WASBKS = LTOPIC(I:I+3).EQ.'\\IT '
            ELSE
               DO 10 J = 1,36
                  IF (LC.EQ.ALPHA(J:J)) GO TO 30
 10               CONTINUE
               END IF
 20         CONTINUE
         LC = ' '
 30      FCH = LC
         END IF
C
 999  RETURN
      END
      SUBROUTINE CHANUM (CHN)
C-----------------------------------------------------------------------
C   changes chapter numbers that are too large to letters
C   In/out:
C      CHN    C*(*)    Chapter number letter code
C-----------------------------------------------------------------------
      CHARACTER CHN*(*)
C
      INTEGER   I, NAPPEN
      PARAMETER (NAPPEN = 10)
      CHARACTER CHLIST(2,NAPPEN)*2
      DATA CHLIST /'21','A', '22','B', '23','C', '24','D', '25','E',
     *   '26','L', '27','F', '30','Z', '40','G', '41','I'/
C-----------------------------------------------------------------------
      DO 10 I = 1,NAPPEN
         IF (CHN.EQ.CHLIST(1,I)) CHN = CHLIST(2,I)
 10      CONTINUE
C
 999  RETURN
      END
