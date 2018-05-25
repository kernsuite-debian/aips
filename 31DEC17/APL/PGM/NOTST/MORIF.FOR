LOCAL INCLUDE 'MORIF.INC'
C                                       Local include for MORIF
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       given time.
      HOLLERITH XNAME1(3), XCLAS1(2), XNAMOU(3), XCLAOU(2)
      REAL     XS1, XDISK1, XBIF, XEIF, XBCHAN, XECHAN, XPIECE, XSOUT,
     *   XDISO, BUFFI(UVBFSL), BUFFO(UVBFSL), FINCI(MAXIF), FINCO(MAXIF)
      DOUBLE PRECISION FOFFI(MAXIF), FOFFO(MAXIF), FRQI(MAXIF),
     *   FRQO(MAXIF), UVWSC
      INTEGER   SEQI, SEQOUT, DISKI, DISKO, LRECI, LRECO, CNOI, NIFI,
     *   INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, NRPRMI, NRPRMO,
     *   NSTOKS, NCHANI, NCHANO, NIFO, JBUFSZ, ILOCWT, ILOCSC,
     *   NVISIN, NVISO, ISBI(MAXIF), ISBO(MAXIF), ORDER(MAXIF), NUMFIL,
     *   NUMVIS, NPIECE, LPIECE, BIF, EIF, BCHAN, ECHAN, NIFT, NCHANT
      CHARACTER NAMEI*12, CLASI*6, NAMOUT*12, CLAOUT*6
      COMMON /INPARM/ XNAME1, XCLAS1, XS1, XDISK1, XBIF, XEIF, XBCHAN,
     *   XECHAN, XPIECE, XNAMOU, XCLAOU, XSOUT, XDISO
      COMMON /EXPARM/ SEQI, SEQOUT, DISKI, DISKO, LRECI, LRECO, CNOI,
     *   NIFI, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, NRPRMI,
     *   NRPRMO, NSTOKS, NCHANI, NCHANO, NIFO, ILOCWT, ILOCSC, NVISIN,
     *   NVISO, NUMFIL, NUMVIS, NPIECE, LPIECE, BIF, EIF, BCHAN, ECHAN,
     *   NIFT, NCHANT
      COMMON /FDATA/ FOFFI, FOFFO, FRQI, FRQO, UVWSC, FINCI, FINCO,
     *   ISBI, ISBO, ORDER
      COMMON /CHARPM/ NAMEI, CLASI, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFFI, BUFFO, JBUFSZ
LOCAL END
LOCAL INCLUDE 'CATS.INC'
      INTEGER   CATI(256), CATO(256)
      REAL      CATRI(256)
      HOLLERITH CATHI(256)
      DOUBLE PRECISION CATDI(128)
      COMMON /CATMAP/ CATI, CATO
      EQUIVALENCE (CATI, CATRI, CATHI, CATDI)
LOCAL END
LOCAL INCLUDE 'CONTROL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFPTR, MAXBAS, OUTSIZ, ICOP, JLOCT, JLOCB, JLOCIT,
     *   ANTMAX
C                                       4X too big
      PARAMETER (MAXBAS = 2*MAXANT*(MAXANT+1))
      LOGICAL   ISCOMP
      REAL      GOTTIM, FINTIM, RINTIM(MAXBAS), BASIN(MAXBAS),
     *   INTTIM(MAXBAS)
      COMMON /VALS/ GOTTIM, FINTIM, RINTIM, BASIN, INTTIM, OUTSIZ,
     *   BUFPTR, ICOP, JLOCT, JLOCB, JLOCIT, ISCOMP, ANTMAX
LOCAL END
      PROGRAM MORIF
C-----------------------------------------------------------------------
C! MORIF breaks data into more or fewer IFs by integer multiples
C# Utility UV UV-util VLA SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 2012-2013, 2015
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
C   MORIF breaks a data set into NPIECE times as many IFs with
C   1/NPIECE as many spectral channels per IF or alternatively 1/NPIECE
C   as many IFs with NPIECE times as many channels
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAME1         Name of input UV data # 1
C      INCLASS        CLAS1         Class of input UV data.
C      INSEQ          SEQ1          Seq. of input UV data.
C      INDISK         DISK1         Disk number of input UV data
C      BIF            BIF           First IF to include
C      EIF            EIF           Last IF to include
C      NPIECE         NPIECE        Number pieces to break each IF
C      OUTNAME        NAMOUT        Name of the output uv file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'MORIF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      DATA PRGM /'MORIF '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file.
      CALL MORIIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy the data
      CALL MORIDA (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL MORIHI
C                                       Then the tables
      CALL MORTAB (IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFI)
C
 999  STOP
      END
      SUBROUTINE MORIIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   MORIIN gets input parameters for MORIF and creates an output file
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                1 => infiles don't match
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, J, K, NFQ, IVER, FREQID,
     *   FQLUN, IFRQ, INUM, NIF
      LOGICAL   T
      DOUBLE PRECISION DFRQ
      INCLUDE 'MORIF.INC'
      INCLUDE 'CATS.INC'
      INCLUDE 'CONTROL.INC'
      CHARACTER BNDCOD(MAXIF)*8, BNDCOO(MAXIF)*8
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 19
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAME1, BUFFI, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFI, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAME1, NAMEI)
      CALL H2CHR (6,  1, XCLAS1, CLASI)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6,  1, XCLAOU, CLAOUT)
      SEQI = IROUND (XS1)
      SEQOUT = IROUND (XSOUT)
      DISKI = IROUND (XDISK1)
      DISKO = IROUND (XDISO)
      IF (XPIECE.LE.0.0) XPIECE = 2
      NPIECE = IROUND (XPIECE)
      IF (NPIECE.LT.2) NPIECE = 1
      LPIECE = IROUND (1.0/XPIECE)
      IF (LPIECE.LT.2) LPIECE = 1
      NIFI = 0
      NVISO = NVIS
C                                       Create new file.
C                                       Get CATBLK from files.
      PTYPE = 'UV'
      LRECI = 0
      CNOI = 1
      CALL CATDIR ('SRCH', DISKI, CNOI, NAMEI, CLASI, SEQI, PTYPE,
     *   NLUSER, STAT, BUFFI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEI, CLASI, SEQI, DISKI, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKI, CNOI, CATI, 'REST', BUFFI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Determine input data file
C                                       characteristics
      CALL COPY (256, CATI, CATBLK)
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI  = LREC
      NRPRMI = NRPARM
      INCSI  = INCS / INCX
      INCFI  = INCF / INCX
      INCIFI = INCIF / INCX
      NVISIN = NVIS
C                                       IFs
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
         NIFI = 1
         NIFT = 1
      ELSE
         BIF = IROUND (XBIF)
         BIF = MAX (1, BIF)
         EIF = IROUND (XEIF)
         NIFT = CATBLK(KINAX+JLOCIF)
         IF (EIF.LT.BIF) EIF = NIFT
         EIF = MIN (NIFT, EIF)
         IF (EIF.LT.BIF) THEN
            BIF = 1
            EIF = NIFT
            END IF
         END IF
      NIFI = EIF - BIF + 1
      NIFO = NIFI
C                                       channels
      NCHANT = CATBLK(KINAX+JLOCF)
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (NCHANT, BCHAN))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = NCHANT
      ECHAN = MIN (ECHAN, NCHANT)
      NCHANI = ECHAN - BCHAN + 1
      NCHANO = NCHANI
C                                       output - set and test
      IF (NPIECE.GT.1) THEN
         NIFO = NIFI * NPIECE
         IF (NIFO.GT.MAXIF) THEN
            MSGTXT = 'MORE IFS REQUESTED THAN AIPS CAN HANDLE'
            CALL MSGWRT (8)
            JERR = 10
            GO TO 999
            END IF
         NCHANO = NCHANI / NPIECE
         IF ((NCHANO*NPIECE.NE.NCHANI) .OR. (NCHANO.LT.2)) THEN
            MSGTXT = 'IMPROPER VALUE OF NPIECE: QUITTING'
            CALL MSGWRT (8)
            JERR = 10
            GO TO 999
            END IF
      ELSE IF (LPIECE.GT.1) THEN
         NIFO = NIFI / LPIECE
         IF (NIFO*LPIECE.NE.NIFI) THEN
            MSGTXT = 'IMPROPER VALUE IF NPIECE: QUITTING'
            CALL MSGWRT (8)
            JERR = 10
            GO TO 999
            END IF
         NCHANO = NCHANI * LPIECE
         END IF
C                                       fix reference pixel
      IF (CATR(KRCRP+JLOCF).NE.BCHAN) THEN
         CATDI(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) +
     *      CATR(KRCIC+JLOCF) * (BCHAN - CATR(KRCRP+JLOCF))
         CATRI(KRCRP+JLOCF) = 1.0
         UVWSC = CATDI(KDCRV+JLOCF) / CATD(KDCRV+JLOCF)
      ELSE
         UVWSC = 1.0D0
         END IF
C                                       Save input CATBLK, => output
      CALL COPY (256, CATI, CATBLK)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP), ILOCSC,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING SCALE FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Get IF info to decide on
C                                       ordering.
      CALL DFILL (MAXIF, -1.D0, FRQO)
      IFRQ = 0
      FQLUN = 40
      IVER = 1
      FREQID = 1
      CALL CHNDAT ('READ', BUFFI, DISKI, CNOI, IVER, CATI, FQLUN,
     *   NIF, FOFFI, ISBI, FINCI, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1190) JERR
         GO TO 990
         END IF
      DO 30 I = 1,NIFI,LPIECE
         K = I + BIF - 1
         FRQI(I) = CATDI(KDCRV+JLOCF) + FOFFI(K)
C                                       adjacent IFs?
         IF (LPIECE.GT.1) THEN
            DO 15 J = 2,LPIECE
               IF ((FINCI(K).NE.FINCI(K+J-1)) .OR.
     *            (ABS((FOFFI(K+J-1)-FOFFI(K+J-2))/FINCI(K)-NCHANT).GT.
     *            0.1)) THEN
                  MSGTXT = 'COMBINED IFS NOT ADJACENT'
                  CALL MSGWRT (8)
                  JERR = 10
                  GO TO 999
                  END IF
 15            CONTINUE
            END IF
         DFRQ = FINCI(K) * NCHANO
         IFRQ = IFRQ + 1
         FRQO(IFRQ) = FRQI(I)
         ISBO(IFRQ)  = ISBI(K)
         FINCO(IFRQ) = FINCI(K)
         BNDCOO(IFRQ) = BNDCOD(K)
         IF (NPIECE.GT.1) THEN
            DO 20 J = 2,NPIECE
               IFRQ = IFRQ + 1
               FRQO(IFRQ) = FRQO(IFRQ-1) + DFRQ
               ISBO(IFRQ)  = ISBI(I)
               FINCO(IFRQ) = FINCI(I)
               BNDCOO(IFRQ) = BNDCOO(I)
 20            CONTINUE
            END IF
 30      CONTINUE
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEI, CLASI, SEQI, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      CATBLK(KINAX+JLOCIF) = NIFO
      CATBLK(KINAX+JLOCF) = NCHANO
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFFO, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
         ELSE
            MSGTXT = 'MORIF MAY NOT OVER-WRITE AN EXISTING DATA SET'
            END IF
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKI, CNOI, NAMEI, CLASI, SEQI, PTYPE,
     *   NLUSER, 'READ', BUFFI, IERR)
      IF (IERR.NE.0) THEN
         JERR = IERR
         WRITE (MSGTXT,1180) IERR, I
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKI
      FCNO(NCFILE) = CNOI
      FRW(NCFILE) = 0
C                                       copy header keywords
      CALL KEYCOP (DISKI, CNOI, DISKO, CCNO, IERR)
C
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
      CALL COPY (256, CATBLK, CATO)
C                                       Ensure all files have FQ tables
      CALL FNDEXT ('FQ', CATI, NFQ)
      IF (NFQ.EQ.0) THEN
         WRITE (MSGTXT,1160) INUM
         JERR = 1
         GO TO 990
         END IF
C                                       Any uvw scaling?
      CATD(KDCRV+JLOCF) = FRQO(1)
      UVWSC = UVWSC * CATD(KDCRV+JLOCF) / CATDI(KDCRV+JLOCF)
C                                       Write the output FQ table
C                                       First fill in the arrays
      DO 300 I = 1,NIFO
         FOFFO(I) = FRQO(I) - CATD(KDCRV+JLOCF)
 300     CONTINUE
      CALL CHNDAT ('WRIT', BUFFO, DISKO, CCNO, IVER, CATBLK, FQLUN,
     *   NIFO, FOFFO, ISBO, FINCO, BNDCOO, FREQID, JERR)
      IF (JERR.NE.0) THEN
         CALL TABERR ('WRIT', 'CHNDAT', 'MORIIN', JERR)
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MORIIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('File ',A12,'.',A6,'.',I4,' on disk ',I3,' covers')
 1020 FORMAT ('time range ',I3,'/',3I3,' - ',
     *   I3,'/',3I3,' and has ',I8,' vis')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1045 FORMAT ('ERROR:',I3,' FREQIDS. CAN ONLY MORIF SINGLE FREQ DATA.')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1100 FORMAT ('MORIIN: FILES 1 & ',I2,': R.P. #',I2,' MISMATCH')
 1110 FORMAT ('MORIIN: FILES 1 & ',I2,': RA/DEC MISMATCH')
 1120 FORMAT ('MORIIN: FILES 1 & ',I2,': # SPECTRAL CHANNELS MISMATCH')
 1130 FORMAT ('MORIIN: FILES 1 & ',I2,': # STOKES MISMATCH')
 1140 FORMAT ('MORIIN: ERROR DETERMING TIME RANGE FOR FILE ',I2)
 1160 FORMAT ('MORIIN: NO FQ TABLE ATTACHED TO FILE ',I2)
 1170 FORMAT ('MORIIN: FILES 1 & ',I2,' DIFFER IN ',A)
 1180 FORMAT ('MORIIN: ERROR ',I3,' CHANGING STATUS OF FILE ',I2)
 1190 FORMAT ('MORIIN: ERROR ',I3,' READING FQ TABLE FROM FILE ',I2)
      END
      SUBROUTINE MORIHI
C-----------------------------------------------------------------------
C   MORIHI copies and updates history file.  It also copies any tables
C   not having IF-dependent columns.
C   Inputs in common (partial)
C      NCFILE    I    Number of catalogue files read or write locked
C                     Will be equal to the number of input files + 1
C      FCNO      I(*) Catalogue numbers for locked files
C                      FCNO(1) is the output file, FCNO(2..NCFILE)
C                      are the input files.
C-----------------------------------------------------------------------
C
      INTEGER   NONOT
      PARAMETER (NONOT = 20)
      CHARACTER NOTTYP(NONOT)*2, HILINE*72
      INTEGER   LUN1, LUNO, IERR
      INCLUDE 'MORIF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUNO /27,28/
      DATA NOTTYP /'FQ','AT','IM','CL','SN','MC','TY','PC','BP','BL',
     *   'SU', 'CQ', 'GC', 'CT', 'FG', 'AN', 'SY', 'CD', 'PD', 'CP'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy history records from first
C                                       input file to output file:
      CALL HISCOP (LUN1, LUNO, DISKI, DISKO, FCNO(2), FCNO(1),
     *   CATBLK, BUFFI, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEI, CLASI, SEQI, DISKI, LUNO, BUFFI, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUNO, BUFFI,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      WRITE (HILINE,1010) TSKNAM, 'BIF', BIF
      CALL HIADD (LUNO, HILINE, BUFFI, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1010) TSKNAM, 'EIF', EIF
      CALL HIADD (LUNO, HILINE, BUFFI, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1010) TSKNAM, 'BCHAN', BCHAN
      CALL HIADD (LUNO, HILINE, BUFFI, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1010) TSKNAM, 'ECHAN', ECHAN
      CALL HIADD (LUNO, HILINE, BUFFI, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUNO, .TRUE., BUFFI, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUNO, DISKI, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFFI, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK for output file.
      CALL CATIO ('UPDT', DISKO, FCNO(1), CATBLK, 'REST', BUFFI, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MORIHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,A,'  =',I8,'   / input data selection')
 1200 FORMAT ('MORIHI: ERROR COPYING TABLES')
      END
      SUBROUTINE MORIDA (IERR)
C-----------------------------------------------------------------------
C   Routine that opens up the input files and the output file and
C   handles the bookkeeping for the glueing process
C   Inputs:
C      NF       I         Number input files
C      LISREC   I(NF,*)   Input rec number for output
C      JR       I         First dimension of inbuff
C   Output:
C      INBUFF   R(JR,*)   I/O buffers
C      IERR     I         Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'MORIF.INC'
      INCLUDE 'CATS.INC'
      INCLUDE 'CONTROL.INC'
      CHARACTER FILIN*48, FILOUT*48
      INTEGER   LUNIN, INDIN, ILENBU, IBIND, OBIND, VO, BO, INIO, NCORI,
     *   NCORO, NICOPY, NOCOPY, OPT, NIOUT, NIOLIM, OLENBU, LUNOUT,
     *   INDOUT, NCNTR, NREINI, RNXRET, IPTRI, III, NIOFF
      REAL      RESULT(UVBFSL), TBUFF(UVBFSL)
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNIN, LUNOUT /40, 41/
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      VO = 0
      BO = 1
      JBUFSZ = UVBFSL * 2
      NREINI = 0
      NCNTR = 0
C                                       Record sizes in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATI(KINAX)
      NICOPY = 3 * INCIFI * NIFI
      NIOFF = 3 * INCIFI * (BIF - 1)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NOCOPY = LRECO - NRPRMO
C                                       Open and init for read
      CALL ZPHFIL ('UV', DISKI, CNOI, 1, FILIN, IERR)
      CALL ZOPEN (LUNIN, INDIN, DISKI, FILIN, T, F, F, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN INPUT'
         GO TO 990
         END IF
      ILENBU = 0
      CALL UVINIT ('READ', LUNIN, INDIN, NVISIN, VO, LRECI, ILENBU,
     *   JBUFSZ, BUFFI, BO, IBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT INPUT'
         GO TO 990
         END IF
C                                       Open the output file
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, FILOUT, IERR)
      CALL ZOPEN (LUNOUT, INDOUT, DISKO, FILOUT, T, F, F, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT'
         GO TO 990
         END IF
      OLENBU = 0
      NVISO = NVISIN
      CALL UVINIT ('WRIT', LUNOUT, INDOUT, NVISO, VO, LRECO, OLENBU,
     *   JBUFSZ, BUFFO, BO, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT OUTPUT'
         GO TO 990
         END IF
      OPT = OBIND
      NIOUT = 0
      NIOLIM = OLENBU
C                                       make an index table
      CALL RNXGET (DISKI, CNOI, CATI)
      CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       start reads
 100  CALL UVDISK ('READ', LUNIN, INDIN, BUFFI, INIO, IBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ INPUT'
         GO TO 990
         END IF
      IF (INIO.GT.0) THEN
         IPTRI = IBIND
         DO 200 III = 1,INIO
            CALL RCOPY (NRPRMO, BUFFI(IPTRI), BUFFO(OBIND))
C                                       uvw scaling
            BUFFO(OBIND+ILOCU) = BUFFO(OBIND+ILOCU) * UVWSC
            BUFFO(OBIND+ILOCV) = BUFFO(OBIND+ILOCV) * UVWSC
            BUFFO(OBIND+ILOCW) = BUFFO(OBIND+ILOCW) * UVWSC
C                                       Compressed
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORI, BUFFI((IPTRI+NRPRMI)),
     *            BUFFI((IPTRI+ILOCWT)), TBUFF)
               CALL SELECT (TBUFF, RESULT)
               CALL ZUVPAK (NCORO, RESULT, BUFFO(OBIND+ILOCWT),
     *            BUFFO(OBIND+NRPRMO))
C                                       Uncompressed
            ELSE
               CALL SELECT (BUFFI(IPTRI+NRPRMI), BUFFO(OBIND+NRPRMO))
               END IF
            NCNTR = NCNTR + 1
C                                       update NX table
            CALL RNXUPD (BUFFO(OBIND), RNXRET)
C                                       update pointers
            OBIND = OBIND + LRECO
            IPTRI = IPTRI + LRECI
            NIOUT = NIOUT + 1
C                                       Write if buffer full
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNOUT, INDOUT, BUFFO, NIOLIM,
     *            OBIND, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT'
                  GO TO 990
                  END IF
               NIOUT = 0
               END IF
 200        CONTINUE
         GO TO 100
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNOUT, INDOUT, BUFFO, NIOUT, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FLUSH OUTPUT'
         GO TO 990
         END IF
C                                       Compress output file.
      CALL UCMPRS (NVISO, DISKO, CCNO, LUNOUT, CATBLK, IERR)
C                                       close NX table
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         CALL MSGWRT (7)
         END IF
C                                       Tidy up
      CALL ZCLOSE (LUNOUT, INDOUT, IERR)
      CALL ZCLOSE (LUNIN, INDIN, IERR)
      WRITE (MSGTXT,1810) NCNTR
      CALL MSGWRT (4)
      IERR = 0
      GO TO 999
C                                       Error
  990 CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MORIDA: ERROR',I3,1X,A4,' FILE ')
 1810 FORMAT ('Wrote ',I8,' visibilities to output file')
      END
      SUBROUTINE SELECT (BUFIN, BUFOU)
C-----------------------------------------------------------------------
C   SELECT copies relevant portions of each input buffer to output
C   Inputs:
C      BUFIN   R(*)   input data buffer (all chans, IFs)
C   Output:
C      BUFOU   R(*)   output
C-----------------------------------------------------------------------
      REAL      BUFIN(*), BUFOU(*)
C
      INTEGER   LIF, INP, OUTP, NCOPY
      INCLUDE 'MORIF.INC'
C-----------------------------------------------------------------------
      NCOPY = 3 * INCFI * (ECHAN - BCHAN + 1)
      OUTP = 1
      DO 10 LIF = BIF,EIF
         INP = (LIF - 1) * INCIFI + (BCHAN - 1) * INCFI
         INP = 1 + 3 * INP
         CALL RCOPY (NCOPY, BUFIN(INP), BUFOU(OUTP))
         OUTP = OUTP + NCOPY
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MORTAB (IERR)
C-----------------------------------------------------------------------
C   Routine that performs the bonding process for tables. Since there
C   are so many tables that need operating on this will do it in a
C   generic sense.
C   Output:
C      IERR   I   Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'MORIF.INC'
      INCLUDE 'CATS.INC'
      INCLUDE 'CONTROL.INC'
      INTEGER   TABDO, MAXIFV, MAXKEY, MAXTWO, MAXIDC
      PARAMETER (TABDO = 18, MAXIFV = 16, MAXKEY = 50)
      PARAMETER (MAXTWO = 10, MAXIDC = 6)
      CHARACTER TABGLU(TABDO)*2, TABIFS(MAXIFV,TABDO)*24,
     *   IFKEY(TABDO)*8, KEYWRD(MAXKEY)*8, TWODIM(MAXTWO,TABDO)*24,
     *   IDKOLS(MAXIDC,TABDO)*24, TABIF1(MAXIFV,6)*24,
     *   TABIF2(MAXIFV,8)*24, TABIF3(MAXIFV,TABDO-14)*24,
     *   IDKOL1(MAXIDC,6)*24, IDKOL2(MAXIDC,6)*24,
     *   IDKOL3(MAXIDC,TABDO-12)*24
      INTEGER   II, NTAB, VER, LUNIN, LUNOUT, NKEY, NREC, NCOL, IROW,
     *   DATP(256), NROWS, COLKEY(TABDO,2), UNIQUE(TABDO), J, NP,
     *   LOGCOL(MAXIFV), BUFFER(512), BUFOUT(512), RECI(XBPRSZ),
     *   RECO(XBPRSZ), DATPO(256), ICOL, LENGTH, RTYPE, IPTR, OPTR,
     *   ITEMP(6), KLOCS(MAXKEY), KVALS(2*MAXKEY), NROWO, KTYP(MAXKEY),
     *   DIMT(TABDO), TWOCOL(MAXTWO), TWOKEY(TABDO,2), TWOLEN,
     *   IDKEY(TABDO), IDUNIQ(TABDO), IDCOL(MAXIDC), IDUM, NKY, NCNTR,
     *   ACOLK, ATWOK, ICTRNO, CTKOLS(12), CTNUMV(12), I, NR, NW,
     *   CHANO(2), IFO(2), IF1, IF2, ITAB
      REAL      RECRI(XBPRSZ), RECRO(XBPRSZ)
      DOUBLE PRECISION RECDI(XBPRSZ/2), RECDO(XBPRSZ/2)
      LOGICAL   WANKOL, TWOD, ISLINE, LTEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INTEGER   MAXFLG, SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2),
     *   LFGRNO
      LOGICAL   PFLAGS(4)
      CHARACTER REASON*24
      REAL      TIMER(2)
      PARAMETER (MAXFLG=1)
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (RECDI, RECI, RECRI)
      EQUIVALENCE (RECDO, RECO, RECRO)
      EQUIVALENCE (TABIFS(1,1), TABIF1(1,1))
      EQUIVALENCE (TABIFS(1,7), TABIF2(1,1))
      EQUIVALENCE (TABIFS(1,15), TABIF3(1,1))
      EQUIVALENCE (IDKOLS(1,1), IDKOL1(1,1))
      EQUIVALENCE (IDKOLS(1,7), IDKOL2(1,1))
      EQUIVALENCE (IDKOLS(1,13), IDKOL3(1,1))
      DATA TABGLU /'AT','IM','CL','SN','MC','TY','PC','BP','BL','SU',
     * 'GC', 'CQ', 'CT', 'AN', 'SY', 'CD', 'PD', 'CP' /
C                                       AT IF columns
      DATA TABIF1 /'POLAA','POLCALA','POLAB','POLCALB',12*' ',
C                                       IM IF columns
     *   'FREQ.VAR','PDELAY_1','PRATE_1','PDELAY_2','PRATE_2',11*' ',
C                                       CL IF columns
     *   'DOPPOFF','REAL1','IMAG1','RATE 1','DELAY 1','WEIGHT 1',
     *   'REFANT 1','REAL2','IMAG2','RATE 2','DELAY 2','WEIGHT 2',
     *   'REFANT 2',3*' ',
C                                       SN IF columns
     *   'REAL1','IMAG1','RATE 1','DELAY 1','WEIGHT 1','REFANT 1',
     *   'REAL2','IMAG2','RATE 2','DELAY 2','WEIGHT 2','REFANT 2',4*' ',
C                                       MC IF columns
     *   'LO_OFFSET_1','DLO_OFFSET_1','LO_OFFSET_2','DLO_OFFSET_2',
     *   12*' ',
C                                       TY IF columns
     *   'TSYS 1','TANT 1','TSYS 2','TANT 2',12*' '/
C                                       PC IF columns
      DATA TABIF2 /'STATE 1','PC_FREQ 1','PC_REAL 1','PC_IMAG 1',
     *   'PC_RATE 1','STATE 2','PC_FREQ 2','PC_REAL 2','PC_IMAG 2',
     *   'PC_RATE 2',6*' ',
C                                       BP IF columns
     *   'WEIGHT 1','REAL 1','IMAG 1','WEIGHT 2','REAL 2','IMAG 2',
     *   10*' ',
C                                       BL IF columns
     *   'REAL M1','IMAG M1','REAL A1','IMAG A1',
     *   'REAL M2','IMAG M2','REAL A2','IMAG A2',8*' ',
C                                       SU IF columns
     *   'IFLUX','QFLUX','UFLUX','VFLUX','FREQOFF','LSRVEL',
     *   'RESTFREQ',9*' ',
C                                       GC IF columns
     *   'TYPE_1','NTERM_1','X_TYP_1','Y_TYP_1','X_VAL_1','Y_VAL_1',
     *   'GAIN_1','SENS_1','TYPE_2','NTERM_2','X_TYP_2','Y_TYP_2',
     *   'X_VAL_2','Y_VAL_2','GAIN_2','SENS_2',
C                                       CQ IF columns
     *   'FFT_SIZE','NO_CHAN','SPEC_AVG','EDGE_FRQ','CHAN_BW',
     *   'TAPER_FN','OVR_SAMP','ZERO_PAD','FILTER','TIME_AVG',
     *   'NO_BITS','FFT_OVLP',4*' ',
C                                       CT IF columns
     *   16*' ',
C                                       AN IF columns
     *   'BEAMFWHM', 'POLCALA', 'POLCALB', 13*' ' /
C                                       SY IF columns
      DATA TABIF3 /'POWER DIF1', 'POWER SUM1', 'POST GAIN1',
     *   'POWER DIF2', 'POWER SUM2', 'POST GAIN2', 10*' ',
C                                       CD IF columns
     *   'TCAL1', 'TCAL2', 14*' ',
C                                       PD IF columns
     *   'P_DIFF', 'REAL 1', 'IMAG 1', 'REAL 2', 'IMAG 2', 11*' ',
C                                       CP IF columns
     *   'I', 'Q', 'U', 'V', 12*' '/
C                                       Columns that are 2-D
C                                       AT table
      DATA TWODIM /'POLCALA','POLCALB',8*' ',
C                                       IM table
     *   'PDELAY_1','PRATE_1','PDELAY_2','PRATE_2',6*' ',
C                                       CL table
     *   10*' ',
C                                       SN table
     *   10*' ',
C                                       MC table
     *   10*' ',
C                                       TY table
     *   10*' ',
C                                       PC table
     *   'STATE 1','PC_FREQ 1','PC_REAL 1','PC_IMAG 1','PC_RATE 1',
     *   'STATE 2','PC_FREQ 2','PC_REAL 2','PC_IMAG 2','PC_RATE 2',
C                                       BP table
     *   'REAL 1','IMAG 1','REAL 2','IMAG 2',6*' ',
C                                       BL table
     *   10*' ',
C                                       SU table
     *   10*' ',
C                                       GC table
     *   'Y_VAL_1','GAIN_1','Y_VAL_2','GAIN_2',6*' ',
C                                       CQ table
     *   'TAPER_FN', 9*' ',
C                                       CT table
     *   10*' ',
C                                       AN table
     *    'POLCALA', 'POLCALB', 8*' ',
C                                       SY table
     *   10*' ',
C                                       CD table
     *   10*' ',
C                                       PD table
     *   'P_DIFF', 'REAL 1', 'IMAG 1', 'REAL 2', 'IMAG 2', 5*' ',

C                                       CP table
     *   'I', 'Q', 'U', 'V', 6*' '/
C                                       # cols for different tables
      DATA COLKEY /2, 3,  7,  6, 2, 2,  5, 3, 4, 7,  8, 12, 12, 3, 3, 1,
     *   3, 4,
     *             4, 5, 13, 12, 4, 4, 10, 6, 8, 7, 16, 12, 12, 3, 6, 2,
     *   5, 4/
C                                       # unique characters for search
      DATA UNIQUE /7, 8, 8,  8, 12, 6, 9, 8, 7, 5, 7, 5, 6, 8, 10, 5, 6,
     *   1/
C                                       # 2-d cols for table
      DATA TWOKEY /1, 2, 0,  0,  0, 0,  5, 2, 0, 0, 2, 1, 0, 2, 0, 0,
     *   3, 4,
     *             2, 4, 0,  0,  0, 0, 10, 4, 0, 0, 4, 1, 0, 2, 0, 0,
     *   5, 4/
C                                       IF keyword for different tables
      DATA IFKEY /'NO_BAND ','NO_BAND ','NO_IF   ','NO_IF   ',
     *   'NO_BAND ','NO_IF   ','NO_BAND ','NO_IF   ','NO_IF   ',
     *   'NO_IF   ','NO_BAND ','NO_IF   ','NO_BAND ', 'NO_IF ',
     *   'NO_IF   ','NO_IF   ','NO_IF   ','NO_IF   ' /
C                                       Cols needed for merging process
C                                       AT table
      DATA IDKOL1 /'TIME','SOURCE_ID','ANTENNA_NO','ARRAY','FREQID',
     *   'TIME_INTERVAL',
C                                       IM table
     *   'TIME','SOURCE_ID','ANTENNA_NO','ARRAY','FREQID',
     *   'TIME_INTERVAL',
C                                       CL table
     *   'TIME','SOURCE ID','ANTENNA NO','SUBARRAY','FREQ ID',
     *   'TIME INTERVAL',
C                                       SN table
     *   'TIME','SOURCE ID','ANTENNA NO','SUBARRAY','FREQ ID',
     *   'TIME INTERVAL',
C                                       MC table
     *   'TIME','SOURCE_ID','ANTENNA_NO','ARRAY','FREQID',' ',
C                                       TY table
     *   'TIME','SOURCE ID','ANTENNA NO','SUBARRAY','FREQ ID',
     *   'TIME INTERVAL'/
C                                       PC table
      DATA IDKOL2 / 'TIME','SOURCE_ID','ANTENNA_NO','ARRAY','FREQID',
     *   'TIME_INTERVAL',
C                                       BP table
     *   'TIME','SOURCE ID','ANTENNA NO','SUBARRAY','FREQ ID',
     *   'INTERVAL',
C                                       BL table
     *   'TIME','SOURCE ID','ANTENNA1','ANTENNA2','FREQ ID',' ',
C                                       SU table
     *   'ID. NO.',5*' ',
C                                       GC table
     *   'ANTENNA_','SUBARRAY','FREQ ID', 3*' ',
C                                       CQ table
     *   'FRQSEL','SUBARRAY', 4*' '/
C                                       CT table
      DATA IDKOL3 / 6*' ',
C                                       AN table
     *   'NOSTA', 5*' ',
C                                       SY table
     *   'TIME', 'SOURCE ID', 'ANTENNA NO.', 'SUBARRAY', 'FREQ ID',
     *   'TIME INTERVAL',
C                                       CD table
     *   'ANTENNA NO.', 'SUBARRAY','FREQ ID', 3*' ',
C                                       PD table
     *   'ANTENNA', 'SUBARRAY ', 'FREQ ID ', 3*' ',
C                                       CP table
     *   'SOURCE', 'SOURCE ID', 4*' '/
C                                       # cols for different tables
      DATA IDKEY /6, 6, 6, 6, 5, 6, 6, 6, 5, 1, 3, 2, 0, 1, 6, 3, 3, 2/
C                                       # unique characters for search
      DATA IDUNIQ /8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 8, 8, 0, 5,
     *   8, 8, 8, 9/
C-----------------------------------------------------------------------
      LUNOUT = 0
      LUNIN = 0
C                                       Loop over file types
      DO 100 II = 1,TABDO
         CALL FNDEXT (TABGLU(II), CATI, NTAB)
         DO 95 VER = 1,NTAB
            NCNTR = 0
            TWOD = .FALSE.
            ISLINE = (TABGLU(II).EQ.'BP') .OR. (TABGLU(II).EQ.'PD') .OR.
     *         (TABGLU(II).EQ.'CP')
            IF (TABGLU(II).EQ.'CT') THEN
               LUNOUT = 40
               LUNIN  = 41
C                                       Copy CT table
               CALL TABCOP ('CT', 1, 1, LUNIN, LUNOUT, DISKI, DISKO,
     *            CNOI, CCNO, CATBLK, BUFFER, BUFOUT, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABERR ('READ', 'TABCOP', 'MORTAB', IERR)
                  LUNIN = 0
                  GO TO 100
                  END IF
C                                        Update number of IFs
               CALL CTINI ('WRIT', BUFOUT, DISKO, CCNO, VER, CATBLK,
     *            LUNOUT, ICTRNO, CTKOLS, CTNUMV, IERR)
               CALL UPDKEY (BUFOUT, IFKEY(II), 4, NIFO, IERR)
               IF (IERR.NE.0) CALL TABERR ('UPDT', 'UPDKEY', 'MORTAB',
     *            IERR)
               CALL TABIO ('CLOS', IDUM, NROWO, BUFOUT, BUFOUT, IERR)
               GO TO 100
               END IF
            IF (NSTOKS.EQ.1) THEN
               ACOLK = COLKEY(II,1)
               ATWOK = TWOKEY(II,1)
            ELSE
               ACOLK = COLKEY(II,2)
               ATWOK = TWOKEY(II,2)
               END IF
C                                       Init table for read
            NP = 0
            LUNIN = 41
            CALL TABINI ('READ', TABGLU(II), DISKI, CNOI, VER, CATI,
     *         LUNIN, NKEY, NREC, NCOL, DATP, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'TABINI', 'MORTAB', IERR)
               LUNIN = 0
               GO TO 90
               END IF
C                                       # rows in table
            NROWS = BUFFER(5)
            NROWO = NROWS
            NP = NP + 1
C                                       For special cases get 2nd
C                                       dimension of IF dependent arrays
            IF (NP.EQ.1) DIMT(II) = 1
            NKY = 1
            KVALS(1) = 0
            KLOCS(1) = 0
C                                       note - trailing blanks are
C                                       significant to TABKEY
            IF (TABGLU(II).EQ.'AT') THEN
               CALL TABKEY ('READ', 'NOPCAL  ', NKY, BUFFER, KLOCS,
     *            KVALS, KTYP, IERR)
            ELSE IF (TABGLU(II).EQ.'IM') THEN
               CALL TABKEY ('READ', 'NPOLY   ', NKY, BUFFER, KLOCS,
     *            KVALS, KTYP, IERR)
            ELSE IF (TABGLU(II).EQ.'PC') THEN
               CALL TABKEY ('READ', 'NO_TONES', NKY, BUFFER, KLOCS,
     *            KVALS, KTYP, IERR)
            ELSE IF (TABGLU(II).EQ.'BP') THEN
               CALL TABKEY ('READ', 'NO_CHAN ', NKY, BUFFER, KLOCS,
     *            KVALS, KTYP, IERR)
            ELSE IF (TABGLU(II).EQ.'PD') THEN
               CALL TABKEY ('READ', 'NO_CHAN ', NKY, BUFFER, KLOCS,
     *            KVALS, KTYP, IERR)
            ELSE IF (TABGLU(II).EQ.'CP') THEN
               CALL TABKEY ('READ', 'NO_CHAN ', NKY, BUFFER, KLOCS,
     *            KVALS, KTYP, IERR)
            ELSE IF (TABGLU(II).EQ.'GC') THEN
               CALL TABKEY ('READ', 'NO_TABS ', NKY, BUFFER, KLOCS,
     *            KVALS, KTYP, IERR)
            ELSE IF (TABGLU(II).EQ.'AN') THEN
               CALL TABKEY ('READ', 'NOPCAL  ', NKY, BUFFER, KLOCS,
     *            KVALS, KTYP, IERR)
C                                       8-char column
C                                       this forces it to be 2-D
            ELSE IF (TABGLU(II).EQ.'CQ') THEN
               TWOD = .TRUE.
               DIMT(II) = 8
               END IF
            IF (IERR.GT.0) THEN
               CALL TABERR ('READ', 'TABKEY', 'MORTAB', IERR)
               GO TO 90
               END IF
            IF ((KLOCS(1).GT.0) .AND. (KTYP(1).EQ.4)) DIMT(II) =
     *         KVALS(KLOCS(1))
            IF (DIMT(II).GT.1) TWOD = .TRUE.
C                                       Find columns for row
C                                       recognition, these are same
C                                       for input & output tables.
            CALL FILL (MAXIDC, 0, IDCOL)
            CALL FNDCOL (IDKEY(II), IDKOLS(1,II), IDUNIQ(II), .TRUE.,
     *         BUFFER, IDCOL, IERR)
            IF (IERR.NE.0) THEN
               IF ((IERR.NE.11) .OR. (TABGLU(II).NE.'AT')) THEN
                  WRITE (MSGTXT,1011) TABGLU(II), 'ID'
                  CALL MSGWRT (8)
                  GO TO 90
C                                       no source column in AT table
               ELSE
                  IERR = 0
                  END IF
               END IF
C                                       Find columns
            CALL FILL (MAXIFV, 0, LOGCOL)
            CALL FNDCOL (ACOLK, TABIFS(1,II), UNIQUE(II),.TRUE.,
     *         BUFFER, LOGCOL, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1011) TABGLU(II), 'IF'
               CALL MSGWRT (8)
               GO TO 90
               END IF
C                                       Find 2-D columns
            CALL FILL (MAXTWO, 0, TWOCOL)
            IF (TWOD) THEN
               CALL FNDCOL (ATWOK, TWODIM(1,II), UNIQUE(II), .TRUE.,
     *            BUFFER, TWOCOL, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1011) TABGLU(II), '2D'
                  CALL MSGWRT (8)
                  GO TO 90
                  END IF
               END IF
C                                       Close to tidy up after FNDCOL
            IDUM = 0
            CALL TABIO ('CLOS', IDUM, NROWS, BUFFER, BUFFER, IERR)
C                                       Reopen for read
            CALL TABINI ('READ', TABGLU(II), DISKI, CNOI, VER, CATI,
     *         LUNIN, NKEY, NREC, NCOL, DATP, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'TABINI', 'MORTAB', IERR)
               LUNIN = 0
               GO TO 90
               END IF
            IF (NROWS.LE.0) THEN
               MSGTXT = 'NO ROWS TO COPY OF TYPE ' // TABGLU(II)
               CALL MSGWRT (8)
               GO TO 90
               END IF
C                                       Open output table for write
C                                       Update DATP array first
            CALL FILL (256, 0, DATPO)
            CALL COPY (128, DATP(129), DATPO(129))
            DO 40 ICOL = 1,NCOL
               IF (WANKOL(ICOL,ACOLK,LOGCOL)) THEN
                  LENGTH = DATPO(128+ICOL) / 10
                  RTYPE = DATPO(128+ICOL) - LENGTH * 10
                  IF (.NOT.ISLINE) THEN
                     LENGTH = LENGTH * NIFO / NIFT
                     DATPO(128+ICOL) = RTYPE + LENGTH * 10
                  ELSE IF (LENGTH.LT.NIFT*NCHANT) THEN
                     LENGTH = LENGTH * NIFO / NIFT
                     DATPO(128+ICOL) = RTYPE + LENGTH * 10
                  ELSE IF (NCHANI.LT.NCHANT) THEN
                     LENGTH = LENGTH / (NCHANT*NIFT)
                     LENGTH = LENGTH * (NIFO * NCHANI)
                     DATPO(128+ICOL) = RTYPE + LENGTH * 10
                     END IF
                  END IF
 40            CONTINUE
C                                       create output table
            NREC = 30
            LUNOUT = 40
            CALL TABINI ('WRIT', TABGLU(II), DISKO, CCNO, VER, CATBLK,
     *         LUNOUT, NKEY, NREC, NCOL, DATPO, BUFOUT, IERR)
            IF (IERR.NE.-1) THEN
               CALL TABERR ('WRIT', 'TABINI', 'MORTAB', IERR)
               LUNOUT = 0
               GO TO 90
               END IF
C                                       Copy keyword/value pairs
            CALL TABKEY ('ALL ', KEYWRD, NKEY, BUFFER, KLOCS, KVALS,
     *         KTYP, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('ALL ', 'TABKEY', 'MORTAB', IERR)
               GO TO 90
               END IF
            CALL TABKEY ('WRIT', KEYWRD, NKEY, BUFOUT, KLOCS, KVALS,
     *         KTYP, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABKEY', 'MORTAB', IERR)
               GO TO 90
               END IF
C                                       Copy col labels
            DO 45 ICOL = 1,NCOL
               IDUM = 3
               CALL TABIO ('READ', IDUM, ICOL, ITEMP, BUFFER, IERR)
               CALL TABIO ('WRIT', IDUM, ICOL, ITEMP, BUFOUT, IERR)
               IDUM = 4
               CALL TABIO ('READ', IDUM, ICOL, ITEMP, BUFFER, IERR)
               CALL TABIO ('WRIT', IDUM, ICOL, ITEMP, BUFOUT, IERR)
 45            CONTINUE
C                                       Update catalogue header
            CALL CATIO ('UPDT', DISKO, CCNO, CATBLK, 'REST', BUFFI,
     *         IERR)
C                                       Update IF keyword
            CALL UPDKEY (BUFOUT, IFKEY(II), 4, NIFO, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('UPDT', 'UPDKEY', 'MORTAB', IERR)
               GO TO 90
               END IF
            IF (ISLINE) THEN
               CALL UPDKEY (BUFOUT, 'NO_CHAN ', 4, NCHANO, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABERR ('UPDT', 'UPDKEY', 'MORTAB', IERR)
                  GO TO 90
                  END IF
               END IF
C                                       Now loop through the rows
C                                       reading and enlarging IF
C                                       dependent ones. Placing
C                                       values from incoming tables in
C                                       the apropriate places in the
C                                       outgoing table.
            DO 70 IROW = 1,NROWO
               NP = 1
               J = IROW
               IDUM = 0
               CALL TABIO ('READ', IDUM, J, RECI, BUFFER, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABERR ('READ', 'TABIO ', 'MORTAB', IERR)
                  GO TO 90
                  END IF
C                                       Run through columns
               DO 50 ICOL = 1,NCOL
C                                       get type, length
                  LENGTH = DATP(128+ICOL) / 10
                  RTYPE = DATP(128+ICOL) - LENGTH * 10
                  IF (LENGTH.GT.0) THEN
                     IPTR = DATP(ICOL)
                     OPTR = DATPO(ICOL)
C                                       bad  type
                     IF ((RTYPE.LT.1) .OR. (RTYPE.GT.7)) THEN
                        WRITE (MSGTXT,1045) TABGLU(II), IROW, ICOL,
     *                     RTYPE
                        CALL MSGWRT (8)
                        IERR = 5
                        GO TO 90
                        END IF
C                                       Straight copy
                     IF (.NOT.WANKOL(ICOL, ACOLK, LOGCOL)) THEN
                        IF (RTYPE.EQ.1) THEN
                           CALL DPCOPY (LENGTH, RECDI(IPTR),
     *                        RECDO(OPTR))
                        ELSE IF (RTYPE.EQ.2) THEN
                           CALL RCOPY (LENGTH, RECRI(IPTR), RECRO(OPTR))
                        ELSE IF (RTYPE.EQ.3) THEN
                           I = (LENGTH + 3) / 4
                           CALL RCOPY (I, RECRI(IPTR), RECRO(OPTR))
                        ELSE IF (RTYPE.GE.4) THEN
                           CALL COPY (LENGTH, RECI(IPTR), RECO(OPTR))
                           END IF
C                                       Reshuffle order
                     ELSE
                        TWOLEN = 1
                        IF (WANKOL(ICOL, ATWOK, TWOCOL))
     *                     TWOLEN = DIMT(II)
                        LTEMP = ISLINE
C                                       trap weight col in BP
                        IF ((ISLINE) .AND. (LENGTH.LT.NCHANT*NIFT)) THEN
                           LTEMP = .FALSE.
                           TWOLEN = 1
                           END IF
                        CALL LOADTB (LTEMP, RTYPE, BCHAN, ECHAN, BIF,
     *                     NPIECE, LPIECE, NIFO, TWOLEN, RECI(IPTR),
     *                     RECO(OPTR), RECRI(IPTR), RECRO(OPTR),
     *                     RECDI(IPTR), RECDO(OPTR))
                        END IF
                     END IF
 50               CONTINUE
C                                       Write output record
               IDUM = 0
               NCNTR = NCNTR + 1
               CALL TABIO ('WRIT', IDUM, IROW, RECO, BUFOUT, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABERR ('WRIT', 'TABIO ', 'MORTAB', IERR)
                  GO TO 999
                  END IF
 70           CONTINUE
C                                       Close 'em down
 90         IDUM = 0
            IF (LUNIN.GT.0) CALL TABIO ('CLOS', IDUM, NROWS, BUFFER,
     *         BUFFER, IERR)
            LUNIN = 0
            IF (LUNOUT.GT.0) CALL TABIO ('CLOS', IDUM, NROWO, BUFOUT,
     *         BUFOUT, IERR)
            LUNOUT = 0
            WRITE (MSGTXT,1095) TABGLU(II), NCNTR, VER
            IF (NCNTR.GT.0) CALL MSGWRT (4)
 95         CONTINUE
 100     CONTINUE
C                                       Do FG tables if any
      MSGTXT = 'Copy/reformat flag tables if any'
      CALL MSGWRT (2)
      CALL FNDEXT ('FG', CATI, NTAB)
      DO 195 ITAB = 1,NTAB
         LUNIN = 41
         CALL FLGINI ('READ', BUFFER, DISKI, CNOI, ITAB, CATI, LUNIN,
     *      IFGRNO, FGKOLS, FGNUMV, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'FLGINI', 'MORTAB', IERR)
            GO TO 195
            END IF
         VER = ITAB
         LUNOUT = 40
         CALL FLGINI ('WRIT', BUFOUT, DISKO, CCNO, VER, CATBLK,
     *      LUNOUT, LFGRNO, FGKOLS, FGNUMV, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('WRIT', 'FLGINI', 'MORTAB', IERR)
            GO TO 190
            END IF
         NROWS = BUFFER(5)
         NR = 0
         NW = 0
         DO 150 IROW = 1,NROWS
            IFGRNO = IROW
            CALL TABFLG ('READ', BUFFER, IFGRNO, FGKOLS, FGNUMV,
     *         SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *         REASON, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('READ', 'TABFLG', 'MORTAB', IERR)
               GO TO 185
            ELSE IF (IERR.EQ.0) THEN
               NR = NR + 1
               IFS(1) = MAX (1, IFS(1))
               IF (IFS(2).LT.IFS(1)) IFS(2) = NIFI
C                                       channels make it complicated
               IF ((CHANS(1).GT.1) .OR. ((CHANS(2).GT.0) .AND.
     *            (CHANS(2).LT.NCHANI))) THEN
                  IF (CHANS(2).LT.CHANS(1)) CHANS(2) = NCHANI
                  IF (NPIECE.GT.1) THEN
                     DO 120 I = IFS(1),IFS(2)
                        IF1 = (CHANS(1) - BCHAN) / NCHANO + 1
                        IF2 = (CHANS(2) - BCHAN) / NCHANO + 1
                        DO 110 II = IF1,IF2
                           IFO(1) = II + (I - 1) * NPIECE
                           IFO(2) = IFO(1)
                           CHANO(1) = CHANS(1) - BCHAN + 1 -
     *                        (II-1)*NCHANO
                           CHANO(2) = CHANS(2) - BCHAN + 1 -
     *                        (II-1)*NCHANO
                           CHANO(1) = MAX (1, CHANO(1))
                           CHANO(2) = MIN (NCHANO, CHANO(2))
                           CALL TABFLG ('WRIT', BUFOUT, LFGRNO, FGKOLS,
     *                        FGNUMV, SOURID, SUBA, FREQID, ANTS, TIMER,
     *                        IFO, CHANO, PFLAGS, REASON, IERR)
                           IF (IERR.NE.0) THEN
                              CALL TABERR ('WRIT', 'TABFLG', 'MORTAB',
     *                           IERR)
                              GO TO 185
                              END IF
 110                       CONTINUE
 120                    CONTINUE
                   ELSE
                     DO 130 I = IFS(1),IFS(2)
                        IFO(1) = (I - 1) / LPIECE + 1
                        IFO(2) = IFO(1)
                        CHANO(1) = CHANS(1) + MOD (I-1,LPIECE) * NCHANI
                        CHANO(2) = CHANS(2) + MOD (I-1,LPIECE) * NCHANI
                        CALL TABFLG ('WRIT', BUFOUT, LFGRNO, FGKOLS,
     *                     FGNUMV, SOURID, SUBA, FREQID, ANTS, TIMER,
     *                     IFO, CHANO, PFLAGS, REASON, IERR)
                        IF (IERR.NE.0) THEN
                           CALL TABERR ('WRIT', 'TABFLG', 'MORTAB',
     *                        IERR)
                           GO TO 185
                           END IF
 130                    CONTINUE
                     END IF
C                                       no channel complication
               ELSE
                  IF (NPIECE.GT.1) THEN
                     IFS(1) = (IFS(1)-1) * NPIECE + 1
                     IFS(2) = IFS(2) * NPIECE
                     IF (CHANS(2).GT.NCHANO) CHANS(2) = NCHANO
                  ELSE
                     IFS(1) = (IFS(1) - 1) / LPIECE + 1
                     IFS(2) = (IFS(2) - 1) / LPIECE + 1
                     IF (CHANS(2).EQ.NCHANI) CHANS(2) = NCHANO
                     END IF
                  NW = NW + 1
                  CALL TABFLG ('WRIT', BUFOUT, LFGRNO, FGKOLS,
     *               FGNUMV, SOURID, SUBA, FREQID, ANTS, TIMER,
     *               IFS, CHANS, PFLAGS, REASON,IERR)
                  IF (IERR.NE.0) THEN
                     CALL TABERR ('WRIT', 'TABFLG', 'MORTAB', IERR)
                     GO TO 185
                     END IF
                  END IF
               END IF
 150        CONTINUE
         WRITE (MSGTXT,1150) NR, ITAB
         CALL MSGWRT (4)
         WRITE (MSGTXT,1151) NW, VER
         CALL MSGWRT (4)
C                                       close out
 185     CALL TABFLG ('CLOS', BUFOUT, LFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, I)
C                                       close in
 190     CALL TABFLG ('CLOS', BUFFER, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, I)
 195     CONTINUE
      IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MORTAB TABLE: ',A2,' COLUMN DIMENSIONS DO NOT MATCH',
     *   2I4)
 1011 FORMAT ('MORTAB TABLE: ',A2,' MISSING REQUIRED ',A,
     *   ' COLUMNS')
 1045 FORMAT ('MORTAB: TABLE ',A2,' ROW ',I6,' COL ',I2,' HAS ILLEGAL',
     *   ' TYPE ',I3)
 1095 FORMAT (A2,' table copied: ',I9,' rows, version',I4)
 1150 FORMAT ('MORTAB: FG file read',I11,' rows, from version',I3)
 1151 FORMAT ('               wrote',I11,' rows,   to version',I3)
      END
      SUBROUTINE UPDKEY (BUFFER, KEYWRD, KEYTYP, KEYVAL, IERR)
C-----------------------------------------------------------------------
C  Routine which updates a keyword-value pairs of an existing
C  calibration table.
C   Inputs:
C     BUFFER      I(*)       Work buffer
C     KEYWRD      C*8        Keyword name
C     KEYTYP      I          Keyword type
C     KEYVAL      I          Keyword value
C   Outputs:
C     IERR        I          Error code, 0 => OK
C                            anything else => problem
C-----------------------------------------------------------------------
      CHARACTER KEYWRD*8
      INTEGER   LOCS, KEYTYP, KEYNUM
      INTEGER KEYVAL, BUFFER(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
      LOCS = 1
      KEYNUM = 1
C
      CALL TABKEY ('WRIT', KEYWRD, KEYNUM, BUFFER, LOCS, KEYVAL, KEYTYP,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UPDKEY: ERROR ',I3,' UPDATING TABLE KEYWORDS')
      END
      LOGICAL FUNCTION WANKOL (ICOL, COLKEY, LOGCOL)
C-----------------------------------------------------------------------
C   WANKOL determines whether a given column is in the list of those
C   dependent on IF.
C  Input:
C    ICOL       I             Column number
C    COLKEY     I             # in LOGCOL
C    LOGCOL     I(*)          List of columns with IF dependency
C-----------------------------------------------------------------------
      INTEGER ICOL, COLKEY, LOGCOL(*), I
C-----------------------------------------------------------------------
      WANKOL = .FALSE.
      DO 100 I = 1, COLKEY
         IF (ICOL.EQ.LOGCOL(I)) THEN
            WANKOL = .TRUE.
            GO TO 999
            END IF
  100    CONTINUE
  999 RETURN
      END
      SUBROUTINE LOADTB (TABTYP, RTYPE, BCHAN, ECHAN, BIF, NPIECE,
     *   LPIECE, NIFO, DIMTWO, BUFFI, BUFFIO, BUFFR, BUFFRO, BUFFD,
     *   BUFFDO)
C-----------------------------------------------------------------------
C  Routine that loads up the output buffer using the IF ORDER
C  defined. Version for binary tables tables.
C   Inputs:
C     TABTYP   L      True => spectral line does not change length
C     RTYPE    I      Data type, 1 = DP, 2 = SP, 3=CHAR, 4 = I
C     NPIECE   I      Number IFs made from each input IF
C     LPIECE   I      Number IFs combined into 1 IF out
C     NIFO     I      # IFs in output file
C     BUFFI    R(*)   Buffer containing input data stream
C     DIMTWO   I      Size of 2nd dimension (in # of RTYPE words),
C                     always is the most rapidly varying.
C   Outputs:
C     BUFFIO   R(*)   Buffer containing output data stream
C-----------------------------------------------------------------------
      LOGICAL   TABTYP
      INTEGER   RTYPE, BCHAN, ECHAN, BIF, NPIECE, LPIECE, NIFO, DIMTWO,
     *   BUFFI(*), BUFFIO(*)
      REAL      BUFFR(*), BUFFRO(*)
      DOUBLE PRECISION BUFFD(*), BUFFDO(*)
C
      INTEGER   I, J, K, L, ISUB, OSUB, LDIM, NIFI, ISUB0, ODIM, ISUM
      REAL      RSUM
      DOUBLE PRECISION DSUM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      LDIM = DIMTWO
      IF (RTYPE.EQ.3) LDIM = (DIMTWO + 3) / 4
      ISUB0 = 1
      IF (TABTYP) THEN
         ISUB0 = BCHAN
         I = ECHAN - BCHAN + 1
         ODIM = I / NPIECE
      ELSE
         ISUB0 = 1
         I = NIFO * LDIM
         ODIM = LDIM
         END IF
      IF (RTYPE.EQ.1) CALL DFILL (I, DBLANK, BUFFDO)
      IF (RTYPE.EQ.2) CALL RFILL (I, FBLANK, BUFFRO)
      IF (RTYPE.EQ.3) CALL RFILL (I, HBLANK, BUFFRO)
      IF (RTYPE.EQ.4) CALL FILL (I, 0, BUFFIO)
      J = 0
      OSUB = 1 - ODIM
      IF (NPIECE.GT.1) THEN
         NIFI = (NIFO / NPIECE) + BIF - 1
         DO 20 I = BIF,NIFI
            ISUB = LDIM * (I-1) + ISUB0
            DO 10 K = 1,NPIECE
               OSUB = OSUB + ODIM
               IF (RTYPE.EQ.1) THEN
                  CALL DPCOPY (ODIM, BUFFD(ISUB), BUFFDO(OSUB))
               ELSE IF ((RTYPE.EQ.2) .OR. (RTYPE.EQ.3)) THEN
                  CALL RCOPY (ODIM, BUFFR(ISUB), BUFFRO(OSUB))
               ELSE IF (RTYPE.EQ.4) THEN
                  CALL COPY (ODIM, BUFFI(ISUB), BUFFIO(OSUB))
                  END IF
               IF (TABTYP) ISUB = ISUB + ODIM
 10            CONTINUE
 20         CONTINUE
      ELSE IF (TABTYP) THEN
         NIFI = NIFO * LPIECE + BIF - 1
         DO 50 I = BIF,NIFI
            ISUB = ODIM * (I-1) + ISUB0
            OSUB = OSUB + ODIM
            IF (RTYPE.EQ.1) THEN
               CALL DPCOPY (ODIM, BUFFD(ISUB), BUFFDO(OSUB))
            ELSE IF ((RTYPE.EQ.2) .OR. (RTYPE.EQ.3)) THEN
               CALL RCOPY (ODIM, BUFFR(ISUB), BUFFRO(OSUB))
            ELSE IF (RTYPE.EQ.4) THEN
               CALL COPY (ODIM, BUFFI(ISUB), BUFFIO(OSUB))
               END IF
 50         CONTINUE
C                                       non spectral merging
      ELSE
         NIFI = NIFO * LPIECE + BIF - 1
         OSUB = 0
         DO 100 I = BIF,NIFI,LPIECE
            DO 90 L = 1,LDIM
               ISUB = LDIM * (I-1) + ISUB0 - LDIM + L - 1
               OSUB = OSUB + 1
               IF (RTYPE.EQ.1) THEN
                  DSUM = 0.0D0
                  DO 60 K = 1,LPIECE
                     ISUB = ISUB + LDIM
                     IF (BUFFD(ISUB).EQ.DBLANK) GO TO 100
                     DSUM = DSUM + BUFFD(ISUB)
 60                  CONTINUE
                  BUFFDO(OSUB) = DSUM / LPIECE
               ELSE IF (RTYPE.EQ.2) THEN
                  RSUM = 0.0
                  DO 70 K = 1,LPIECE
                     ISUB = ISUB + LDIM
                     IF (BUFFR(ISUB).EQ.FBLANK) GO TO 100
                     RSUM = RSUM + BUFFR(ISUB)
 70                  CONTINUE
                  BUFFRO(OSUB) = RSUM / LPIECE
               ELSE IF (RTYPE.EQ.3) THEN
                  ISUB = ISUB + LDIM
                  CALL RCOPY (ODIM, BUFFR(ISUB), BUFFRO(OSUB))
               ELSE IF (RTYPE.EQ.4) THEN
                  ISUM = 0.0
                  DO 80 K = 1,LPIECE
                     ISUM = ISUM + BUFFI(ISUB)
 80                  CONTINUE
                  BUFFIO(OSUB) = ISUM / LPIECE
                  END IF
 90            CONTINUE
 100        CONTINUE
         END IF
C
 999  RETURN
      END
