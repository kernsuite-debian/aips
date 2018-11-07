LOCAL INCLUDE 'VBGLU.INC'
C                                       Local include for VBGLU
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LARGER
      PARAMETER (LARGER = 4)
C                                       The output buffer has to be
C                                       large enough to hold all
C                                       spectral channels for all
C                                       baselines of the VLBA at any
C                                       given time.
      HOLLERITH XNAME1(3), XCLAS1(2), XNAME2(3), XCLAS2(2), XNAME3(3),
     *   XCLAS3(2), XNAME4(3), XCLAS4(2), XNAMOU(3), XCLAOU(2)
      REAL     XS1, XDISK1, XS2, XDISK2, XS3, XDISK3, XS4, XDISK4,
     *   XSOUT, XDISO, XCENT, BUFFI(LARGER*UVBFSL,4), BUFFO(UVBFSL),
     *   TBI(4), TSI(4), FINCI(MAXIF,4), FINCO(MAXIF), DIFPIX(4)
      DOUBLE PRECISION FOFFI(MAXIF,4), FOFFO(MAXIF), FRQI(MAXIF,4),
     *   FRQO(MAXIF), UVWSC(4)
      INTEGER   SEQI(4), SEQOUT, DISKI(4), DISKO, LRECI(4), LRECO,
     *   CNOI(4), NIFI(4), INCSI(4), INCFI(4), INCIFI(4), INCSO, INCFO,
     *   INCIFO, NRPRMI(4), NRPRMO, NSTOKS, NCHAN, NIFO, NUMHIS, JBUFSZ,
     *   ILOCWT, ILOCSC, NVISIN(4), NVISO, ISBI(MAXIF,4), ISBO(MAXIF),
     *   ORDER(MAXIF), NUMFIL, NUMVIS(4)
      LOGICAL   GLU3, GLU4, SORTED
      CHARACTER NAMEI(4)*12, CLASI(4)*6, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64, BNDCOD(MAXIF,4)
      COMMON /INPARM/ XNAME1, XCLAS1, XS1, XDISK1,
     *   XNAME2, XCLAS2, XS2, XDISK2,
     *   XNAME3, XCLAS3, XS3, XDISK3,
     *   XNAME4, XCLAS4, XS4, XDISK4,
     *   XNAMOU, XCLAOU, XSOUT, XDISO, XCENT
      COMMON /VBGLUP/ SEQI, SEQOUT, DISKI, DISKO, LRECI, LRECO, CNOI,
     *   NIFI, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, NRPRMI,
     *   NRPRMO, NSTOKS, NCHAN, NIFO, NUMHIS, ILOCWT, ILOCSC, NVISIN,
     *   NVISO, NUMFIL, NUMVIS, GLU3, GLU4, SORTED, DIFPIX
      COMMON /FDATA/ FOFFI, FOFFO, FRQI, FRQO, UVWSC,
     *   FINCI, FINCO, TBI, TSI, ISBI, ISBO, ORDER
      COMMON /CHARPM/ NAMEI, CLASI, NAMOUT, CLAOUT, HISCRD, BNDCOD
      COMMON /BUFRS/ BUFFI, BUFFO, JBUFSZ
LOCAL END
LOCAL INCLUDE 'CATS.INC'
      INTEGER   CATI(256,4), CATO(256)
      REAL      CATRI(256,4)
      HOLLERITH CATHI(256,4)
      DOUBLE PRECISION CATDI(128,4)
      COMMON /CATMAP/ CATI, CATO
      EQUIVALENCE (CATI, CATRI, CATHI, CATDI)
LOCAL END
LOCAL INCLUDE 'CONTROL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFPTR, MAXBAS, OUTSIZ, ICOP, ANTMAX
C                                       4X too big
      PARAMETER (MAXBAS = 2*MAXANT*(MAXANT+1))
      LOGICAL   ISCOMP
      REAL      GOTTIM, FINTIM, RINTIM(MAXBAS), BASIN(MAXBAS),
     *   INTTIM(MAXBAS)
      COMMON /VALS/ GOTTIM, FINTIM, RINTIM, BASIN, INTTIM, OUTSIZ,
     *   BUFPTR, ICOP, ISCOMP, ANTMAX
LOCAL END
LOCAL INCLUDE 'TIMLST.INC'
      LONGINT   OFFSET, POFSET, DFFSET
      INTEGER   NWORDS, PWORDS
      INTEGER   LISTIM(2), LISREC(2)
      REAL      RLISTM(2)
      DOUBLE PRECISION DLISTM(2)
      EQUIVALENCE (LISTIM, DLISTM, RLISTM)
      COMMON /TIMLST/ OFFSET, POFSET, DFFSET, NWORDS, PWORDS, LISTIM,
     *   LISREC
LOCAL END
      PROGRAM VBGLU
C-----------------------------------------------------------------------
C!  VBGLU Glues together data from multiple passes thru the VLBA corr.
C# Utility UV UV-util VLA VLB SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2001, 2006-2012, 2014-2017
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
C   VBGLU Glues together data that were correlated in multiple passes
C   on the VLBA correlator, multiple pass is not used in its historical
C   sense of different baselines/pass but in the sense of different
C   IFs/pass. This is necessary because the VLBA correlator can only
C   deal with 8 IFs at a time, but when MkIII Mode A,B or C are
C   correlated the data have to be split up in frequency space.
C
C   VBGLU is a generalization of UVGLU. What it does is read the file
C   headers and FQ tables and constructs a composite header/FQ table.
C   It then uses the first file given by the user as the master file and
C   copies that file to output, exapnding it to the appropriate number
C   of IFs. It then runs through the (up to 3) other files joining
C   together spectra with the same baseline/time. If a record exists
C   with no match in the master output file then it will be dropped.
C   If there are records in the mastre output file with no match in the
C   other input files then the empty IFs will be flagged.
C
C   Several assumptions are implicit in this process:
C   (1) Data have the same order
C   (2) The source/antenna numbers are identical
C   (3) There are the same number of spectral channels per IF
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAME1         Name of input UV data # 1
C      INCLASS        CLAS1         Class of input UV data.
C      INSEQ          SEQ1          Seq. of input UV data.
C      INDISK         DISK1         Disk number of input UV data
C      IN2NAME        NAME2         Name of second file to glue
C      IN2CLASS       CLAS2         Class of second file
C      IN2SEQ         SEQ2          Seq number of second file.
C      IN2DISK        DISK2         Vol. no. of second file.
C      IN3NAME        NAME3         Name of third file to glue
C      IN3CLASS       CLAS3         Class of third file
C      IN3SEQ         SEQ3          Seq number of third file.
C      IN3DISK        DISK3         Vol. no. of third file.
C      IN4NAME        NAME4         Name of fourth file to glue
C      IN4CLASS       CLAS4         Class of fourth file
C      IN4SEQ         SEQ4          Seq number of fourth file.
C      IN4DISK        DISK4         Vol. no. of fourth file.
C      OUTNAME        NAMOUT        Name of the output uv file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET, JR
      INCLUDE 'VBGLU.INC'
      INCLUDE 'TIMLST.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      DATA PRGM /'VBGLU '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file.
      CALL VBGLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL ZMEMRY ('FREE', TSKNAM, NWORDS, LISTIM, OFFSET, IRET)
C                                       Glue the data together
      JR = LARGER * 4 * UVBFSL / NUMFIL
      CALL GLUDAT (NUMFIL, LISREC(1+POFSET), JR, BUFFI, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL ZMEMRY ('FREE', TSKNAM, PWORDS, LISREC, POFSET, IRET)
      CALL VBGHIS
C                                       Then the tables
      CALL GLUTAB (IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFI(1,1))
C
 999  STOP
      END
      SUBROUTINE VBGLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   VBGLIN gets input parameters for VBGLU and creates an output file
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
      CHARACTER  STAT*4, BLANK*6, PTYPE*2, SORT1*2, SORT2*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, J, ILOCDE, ILOCRA, ILOCIF,
     *   ILOCF, ILOCS, NFQ, TIM1(4), TIM2(4), IVER, FREQID, FQLUN, IFRQ,
     *   INUM, NUMAN(513), ALUN, NKEY, NREC, NCOL, DATP(256), NFQID,
     *   BUFFER(512), IDUM
      LOGICAL   T, F, WEQ1, FAIL
      INCLUDE 'VBGLU.INC'
      INCLUDE 'CATS.INC'
      INCLUDE 'CONTROL.INC'
      CHARACTER FQBCOD(MAXIF)*8
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
      DATA T, F /.TRUE.,.FALSE./
      DATA ALUN /29/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 36
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
      CALL H2CHR (12, 1, XNAME1, NAMEI(1))
      CALL H2CHR (6,  1, XCLAS1, CLASI(1))
      CALL H2CHR (12, 1, XNAME2, NAMEI(2))
      CALL H2CHR (6,  1, XCLAS2, CLASI(2))
      CALL H2CHR (12, 1, XNAME3, NAMEI(3))
      CALL H2CHR (6,  1, XCLAS3, CLASI(3))
      CALL H2CHR (12, 1, XNAME4, NAMEI(4))
      CALL H2CHR (6,  1, XCLAS4, CLASI(4))
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6,  1, XCLAOU, CLAOUT)
      SEQI(1) = IROUND (XS1)
      SEQI(2) = IROUND (XS2)
      SEQI(3) = IROUND (XS3)
      SEQI(4) = IROUND (XS4)
      SEQOUT = IROUND (XSOUT)
      DISKI(1) = IROUND (XDISK1)
      DISKI(2) = IROUND (XDISK2)
      DISKI(3) = IROUND (XDISK3)
      DISKI(4) = IROUND (XDISK4)
      DISKO = IROUND (XDISO)
      GLU3 = F
      GLU4 = F
      CALL FILL (4, 0, NIFI)
      NVISO = 0
C                                       Create new file.
C                                       Get CATBLK from files.
      PTYPE = 'UV'
      DO 20 I = 1,4
         LRECI(I) = 0
         IF (NAMEI(I).NE.' ') THEN
            CNOI(I) = 1
            CALL CATDIR ('SRCH', DISKI(I), CNOI(I), NAMEI(I), CLASI(I),
     *         SEQI(I), PTYPE, NLUSER, STAT, BUFFI, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, NAMEI(I), CLASI(I), SEQI(I),
     *            DISKI(I), NLUSER
               GO TO 990
               END IF
            CALL CATIO ('READ', DISKI(I), CNOI(I), CATI(1,I), 'REST',
     *         BUFFI, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) IERR
               GO TO 990
               END IF
C                                       Determine input data file
C                                       characteristics
            CALL COPY (256, CATI(1,I), CATBLK)
            CALL UVPGET (JERR)
            IF (JERR.NE.0) GO TO 999
            IF (I.EQ.1) THEN
               J = ILOCB
            ELSE IF (ILOCB.NE.J) THEN
               MSGTXT = 'CANNOT HANDLE MIXED BASELINE TYPES'
               IERR = 8
               GO TO 990
               END IF
C                                       Save input file info
            INCX = CATBLK(KINAX)
            LRECI(I)  = LREC
            NRPRMI(I) = NRPARM
            INCSI(I)  = INCS / INCX
            INCFI(I)  = INCF / INCX
            INCIFI(I) = INCIF / INCX
            NVISIN(I) = NVIS
C                                       fix reference pixel
            IF (JLOCF.LT.0) XCENT = -1.
            IF (XCENT.GT.0.0) THEN
               INCX = CATBLK(KINAX+JLOCF) / 2 + 1
            ELSE
               INCX = 1
               END IF
            IF (CATR(KRCRP+JLOCF).NE.INCX) THEN
               DIFPIX(I) = INCX - CATR(KRCRP+JLOCF)
               CATDI(KDCRV+JLOCF,I) = CATD(KDCRV+JLOCF) +
     *            CATR(KRCIC+JLOCF) * DIFPIX(I)
               CATRI(KRCRP+JLOCF,I) = INCX
               UVWSC(I) = CATDI(KDCRV+JLOCF,I) / CATD(KDCRV+JLOCF)
            ELSE
               UVWSC(I) = 1.0D0
               DIFPIX(I) = 0.0
               END IF
            END IF
   20    CONTINUE
C                                       Save input CATBLK, => output
      CALL COPY (256, CATI(1,1), CATBLK)
      CALL UVPGET (JERR)
C                                       Are there 3rd and 4th files,
C                                       NAMEI(3) & NAMEI(4) should be non
C                                       blank
      NUMFIL = 2
      IF (NAMEI(3).NE.' ') THEN
         GLU3 = T
         NUMFIL = 3
         IF (NAMEI(4).NE.' ') THEN
            GLU4 = T
            NUMFIL = 4
            END IF
         END IF
C                                       Find number of FQ, AN
      ANTMAX = 0
      DO 30 I = 1,NUMFIL
C                                       max antenna number
C                                       and re-format if needed
         CALL GETNAN (DISKI(I), CNOI(I), CATI(1,I), ALUN, BUFFI, NUMAN,
     *      IERR)
         ANTMAX = MAX (ANTMAX, NUMAN(2))

         FQLUN = 40
         NREC=0
         NCOL=0
         NKEY=0
         CALL TABINI ('READ','FQ', DISKI(I), CNOI(I), 1, CATBLK, FQLUN,
     *      NKEY, NREC, NCOL, DATP, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABINI', 'VBGLIN', IERR)
            JERR = 9
            GO TO 990
            END IF
C                                        # rows/FQID in table
C                                        If more than one FQID die
         NFQID = BUFFER(5)
         CALL TABIO ('CLOS', IDUM, IDUM, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('CLOS', 'TABINI', 'VBGLIN', IERR)
            JERR = 9
            GO TO 990
            END IF
         IF (NFQID.NE.1) THEN
            WRITE (MSGTXT,1045) NFQID
            JERR = 9
            GO TO 990
            END IF
 30      CONTINUE

C                                       Set up some indices
      CALL AXEFND (8, 'DEC     ', CATI(KIDIM,1), CATHI(KHCTP,1),
     *   ILOCDE, JERR)
      CALL AXEFND (8, 'RA      ', CATI(KIDIM,1), CATHI(KHCTP,1),
     *   ILOCRA, JERR)
      CALL AXEFND (8, 'STOKES  ', CATI(KIDIM,1), CATHI(KHCTP,1),
     *   ILOCS, JERR)
      IF (JERR.NE.0) THEN
         NSTOKS = 0
      ELSE
         NSTOKS = CATI(KINAX+ILOCS,1)
         END IF
      CALL AXEFND (8, 'IF      ', CATI(KIDIM,1), CATHI(KHCTP,1),
     *   ILOCIF, JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'NO IF AXIS - NOTHING FOR ME TO DO'
         JERR = 1
         GO TO 990
      ELSE
         NIFI(1) = CATI(KINAX+ILOCIF,1)
         NIFI(2) = CATI(KINAX+ILOCIF,2)
         NIFI(3) = CATI(KINAX+ILOCIF,3)
         NIFI(4) = CATI(KINAX+ILOCIF,4)
         END IF
      CALL AXEFND (8, 'FREQ    ', CATI(KIDIM,1), CATHI(KHCTP,1),
     *   ILOCF, JERR)
      IF (JERR.NE.0) THEN
         NCHAN = 0
      ELSE
         NCHAN = CATI(KINAX+ILOCF,1)
         END IF
C                                       Check observing date, #rps,
C                                       # complex axes, sort order
C                                       File 1 & 2
      FAIL = .FALSE.
      CALL H2CHR (2, 1, CATHI(KITYP,1), SORT1)
      DO 50 INUM = 2,NUMFIL
C                                       Compare Hollerith strings
         CALL CHCOMP (8, 1, CATHI(KHDOB,1), 1, CATHI(KHDOB,INUM), WEQ1)
         IF (.NOT.WEQ1) THEN
            WRITE (MSGTXT,1170) INUM, 'OBSERVATION DATE'
            CALL MSGWRT (8)
            FAIL = .TRUE.
            END IF
         CALL H2CHR (2, 1, CATHI(KITYP,INUM), SORT2)
         IF ((SORT1.NE.SORT2) .AND. ((SORT1(:1).NE.'T') .OR.
     *      (SORT2(:1).NE.'T'))) THEN
            WRITE (MSGTXT,1170) INUM, 'SORT ORDER'
            CALL MSGWRT (8)
            FAIL = .TRUE.
            END IF
         IF (CATI(KIPCN,1).NE.CATI(KIPCN,INUM)) THEN
            WRITE (MSGTXT,1170) INUM, 'PARAMETER COUNT'
            CALL MSGWRT (8)
            FAIL = .TRUE.
            END IF
         IF (CATI(KINAX,1).NE.CATI(KINAX,INUM)) THEN
            WRITE (MSGTXT,1170) INUM, 'PARAMETER COUNT'
            CALL MSGWRT (8)
            FAIL = .TRUE.
            END IF
         IF (FAIL) THEN
            JERR = 1
            GO TO 999
            END IF
C                                       Check random parameters
         DO 40 J = 1,CATI(KIPCN,1)
C                                       Compare Hollerith strings
            CALL CHCOMP (8, 1, CATHI((KHPTP+(I-1)*2),1), 1,
     *         CATHI((KHPTP+(I-1)*2),INUM), WEQ1)
            IF (.NOT.WEQ1) THEN
               WRITE (MSGTXT,1100) INUM, J
               JERR = 1
               GO TO 990
               END IF
 40         CONTINUE
C                                       Check coordinates
         IF ((ABS(CATDI(KDCRV+ILOCDE,1)-CATDI(KDCRV+ILOCDE,INUM))
     *      .GT.0.01)  .OR. (ABS(CATDI(KDCRV+ILOCRA,1) -
     *      CATDI(KDCRV+ILOCRA,INUM))).GT.0.01) THEN
            WRITE (MSGTXT,1110) INUM
            JERR = 1
            GO TO 990
            END IF
C                                       Make sure # stokes & spectral
C                                       channels the same. Store
C                                       # IFs.
C                                       # channels
         IF (NCHAN.NE.CATI(KINAX+ILOCF,INUM)) THEN
            WRITE (MSGTXT,1120) INUM
            JERR = 1
            GO TO 990
            END IF
C                                       # Stokes
         IF (NSTOKS.NE.CATI(KINAX+ILOCS,INUM)) THEN
            WRITE (MSGTXT,1130) INUM
            JERR = 1
            GO TO 990
            END IF
 50      CONTINUE
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
C                                       go find the UV data times
      CALL GETIMS (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       center frequencies
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEI(1), CLASI(1), SEQI(1), BLANK, NAMOUT,
     *   CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       sort order out is not known
C                                       start with guess
      CALL CHR2H (4, 'TB  ', 1, CATBLK(KITYP))
C                                       Calculate # output IFs
      NIFO = NIFI(1) + NIFI(2) + NIFI(3) + NIFI(4)
      CATBLK(KINAX+ILOCIF) = NIFO
      CATBLK(KIGCN) = NVISO
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFFO, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
         ELSE
            MSGTXT = 'VBGLU MAY NOT OVER-WRITE AN EXISTING DATA SET'
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
      DO 80 I = 1, NUMFIL
         CALL CATDIR ('CSTA', DISKI(I), CNOI(I), NAMEI(I), CLASI(I),
     *      SEQI(I), PTYPE, NLUSER, 'READ', BUFFI, IERR)
         IF (IERR.NE.0) THEN
            JERR = IERR
            WRITE (MSGTXT,1180) IERR, I
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKI(I)
         FCNO(NCFILE) = CNOI(I)
         FRW(NCFILE) = 0
 80      CONTINUE
C                                       copy header keywords
      CALL KEYCOP (DISKI(1), CNOI(1), DISKO, CCNO, IERR)
C
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Check start & stop times of
C                                       each file to ensure some overlap
C                                       Also determine which one starts
C                                       first.
C                                       If no NX must have DUVH set
      CALL COPY (256, CATBLK, CATO)
      DO 130 INUM = 1,NUMFIL
         CALL COPY (256, CATI(1,INUM), CATBLK)
         CALL UVPGET (JERR)
         CALL UVTIME (DISKI(INUM), CNOI(INUM), CATI(1,INUM), TBI(INUM),
     *      TSI(INUM), JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1140) INUM
            GO TO 990
            END IF
         CALL TODHMS (TBI(INUM), TIM1)
         CALL TODHMS (TSI(INUM), TIM2)
         WRITE (MSGTXT,1010) NAMEI(INUM), CLASI(INUM), SEQI(INUM),
     *      DISKI(INUM)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1020) TIM1, TIM2, CATI(KIGCN,INUM)
         CALL MSGWRT (4)
 130     CONTINUE
      CALL COPY (256, CATO, CATBLK)
      CALL UVPGET (JERR)
C                                       Ensure all files have FQ tables
      DO 160 INUM = 1,NUMFIL
         CALL FNDEXT ('FQ', CATI(1,INUM), NFQ)
         IF (NFQ.EQ.0) THEN
            WRITE (MSGTXT,1160) INUM
            JERR = 1
            GO TO 990
            END IF
 160     CONTINUE
C                                       Get IF info to decide on
C                                       ordering.
      CALL DFILL (MAXIF, -1.D0, FRQO)
      CALL FILL (MAXIF, 0, ORDER)
      IFRQ = 0
      FQLUN = 40
      IVER = 1
      FREQID = 1
      DO 220 INUM = 1,NUMFIL
         CALL CHNDAT ('READ', BUFFI(1,INUM), DISKI(INUM), CNOI(INUM),
     *      IVER, CATI(1,INUM), FQLUN, NIFI(INUM), FOFFI(1,INUM),
     *      ISBI(1,INUM), FINCI(1,INUM), BNDCOD(1,INUM), FREQID, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1190) JERR, INUM
            GO TO 990
            END IF
         DO 200 I = 1,NIFI(INUM)
            FRQI(I,INUM) = CATDI(KDCRV+JLOCF,INUM) + FOFFI(I,INUM) +
     *         DIFPIX(I) * (FINCI(I,INUM) - CATR(KRCIC+JLOCF))
            IFRQ = IFRQ + 1
            FRQO(IFRQ) = FRQI(I,INUM)
 200        CONTINUE
 220     CONTINUE
C                                       Determine the output order
      CALL DETORD (FRQO, NIFO, ORDER)
      DO 230 I = 1, NIFO
         IF (ORDER(I).LE.NIFI(1)) THEN
            ORDER(I) = ORDER(I) + 100
         ELSE IF (ORDER(I).LE.NIFI(1)+NIFI(2)) THEN
            ORDER(I) = ORDER(I) - NIFI(1) + 200
         ELSE IF ((GLU3) .AND.
     *      (ORDER(I).LE.NIFI(1)+NIFI(2)+NIFI(3))) THEN
            ORDER(I) = ORDER(I) - (NIFI(1)+NIFI(2)) + 300
         ELSE IF ((GLU4) .AND.
     *      (ORDER(I).LE.NIFI(1)+NIFI(2)+NIFI(3)+NIFI(4))) THEN
             ORDER(I) = ORDER(I) - (NIFI(1) + NIFI(2) + NIFI(3)) + 400
            END IF
 230     CONTINUE
C                                       Any uvw scaling?
      CATD(KDCRV+JLOCF) = FRQO(1)
      DO 280 INUM = 1, NUMFIL
         UVWSC(INUM) = UVWSC(INUM) * CATD(KDCRV+JLOCF) /
     *      CATDI(KDCRV+JLOCF,INUM)
 280     CONTINUE
C                                       Write the output FQ table
C                                       First fill in the arrays
      DO 300 I = 1, NIFO
         CALL GETORD (ORDER, I, INUM, J)
         FOFFO(I) = FRQI(J,INUM) - CATD(KDCRV+JLOCF)
         ISBO(I)  = ISBI(J,INUM)
         FINCO(I) = FINCI(J,INUM)
         FQBCOD(I) = BNDCOD(J,INUM)
 300     CONTINUE
      CALL CHNDAT ('WRIT', BUFFO, DISKO, CCNO, IVER, CATBLK, FQLUN,
     *   NIFO, FOFFO, ISBO, FINCO, FQBCOD, FREQID, JERR)
      IF (JERR.NE.0) THEN
         CALL TABERR ('WRIT', 'CHNDAT', 'VBGLIN', JERR)
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VBGLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('File ',A12,'.',A6,'.',I4,' on disk ',I3,' covers')
 1020 FORMAT ('time range ',I3,'/',3I3,' - ',
     *   I3,'/',3I3,' and has ',I8,' vis')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1045 FORMAT ('ERROR:',I3,' FREQIDS. CAN ONLY VBGLU SINGLE FREQ DATA.')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1100 FORMAT ('VBGLIN: FILES 1 & ',I2,': R.P. #',I2,' MISMATCH')
 1110 FORMAT ('VLBGIN: FILES 1 & ',I2,': RA/DEC MISMATCH')
 1120 FORMAT ('VLBGIN: FILES 1 & ',I2,': # SPECTRAL CHANNELS MISMATCH')
 1130 FORMAT ('VLBGIN: FILES 1 & ',I2,': # STOKES MISMATCH')
 1140 FORMAT ('VLBGIN: ERROR DETERMING TIME RANGE FOR FILE ',I2)
 1160 FORMAT ('VLBGIN: NO FQ TABLE ATTACHED TO FILE ',I2)
 1170 FORMAT ('VLBGIN: FILES 1 & ',I2,' DIFFER IN ',A)
 1180 FORMAT ('VLBGIN: ERROR ',I3,' CHANGING STATUS OF FILE ',I2)
 1190 FORMAT ('VLBGIN: ERROR ',I3,' READING FQ TABLE FROM FILE ',I2)
      END
      SUBROUTINE DETORD (BANDFR, NOBAND, ORDER)
C-----------------------------------------------------------------------
C  Routine to determine the order of frequencies.
C  Input:
C     BANDFR     D(*)      IF sky freqs
C     NOBAND     I         # IF'S
C  Output:
C     ORDER      I(*)      Order of frequencies
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION BANDFR(MAXIF), T1
      INTEGER NOBAND, ORDER(MAXIF), I, J, K, II
C-----------------------------------------------------------------------
C                                       Sort them into increasing order,
C                                       ensuring deal with offsets that
C                                       are the same.
      DO 10 I = 1, NOBAND
         ORDER(I) = I
   10    CONTINUE
C
      K = NOBAND - 1
      DO 300 II = 1, NOBAND
         DO 250 I = 1, K
            IF (BANDFR(I+1).LT.BANDFR(I)) THEN
               J = ORDER(I)
               ORDER(I) = ORDER(I+1)
               ORDER(I+1) = J
               T1 = BANDFR(I)
               BANDFR(I) = BANDFR(I+1)
               BANDFR(I+1) = T1
               END IF
 250        CONTINUE
 300     CONTINUE
C
      RETURN
      END
      SUBROUTINE GETIMS (IERR)
C-----------------------------------------------------------------------
C   Gets all of the time/baseline pairs for all 4 data sets including
C   allocating the dynamic memory for it
C   Outputs in common
C      NVISO    I   Number of vis in output file
C   Outputs:
C      IERR     I   Error
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      INCLUDE 'VBGLU.INC'
      INCLUDE 'CATS.INC'
      INCLUDE 'TIMLST.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       dynamic memory needed
      NWORDS = 0
      DO 10 I = 1,NUMFIL
         NWORDS = MAX (NWORDS, CATI(KIGCN,I))
 10      CONTINUE
      NWORDS = (NUMFIL * 3 * NWORDS - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, LISTIM, OFFSET, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'FAILED TO GET DYNAMIC MEMORY FOR UV TIME LIST'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      DFFSET = (OFFSET + 1) / 2
      CALL FILL (1024*NWORDS, 0, RLISTM(1+OFFSET))
C                                       loop over files
      SORTED = .TRUE.
      DO 20 I = 1,NUMFIL
         CALL GETIM1 (I, NUMFIL, RLISTM(1+OFFSET), IERR)
         IF (IERR.NE.0) GO TO 999
 20      CONTINUE
C                                       count
      CALL CNTIMS (SORTED, NUMVIS, NUMFIL, RLISTM(1+OFFSET),
     *   LISTIM(1+OFFSET), NVISO)
C                                       sort
      PWORDS = (NUMFIL * NVISO - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, PWORDS, LISREC, POFSET, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'FAILED TO GET DYNAMIC MEMORY FOR SORTED LIST'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL FILL (1024*PWORDS, 0, LISREC(1+POFSET))
      CALL SRTIMS (3, NUMVIS, NUMFIL, LISTIM(1+OFFSET), NVISO,
     *   LISREC(1+POFSET))
C
 999  RETURN
      END
      SUBROUTINE GETIM1 (II, NF, LISTIM, IERR)
C-----------------------------------------------------------------------
C   Get the times/baselines of 1 data set
C   Inputs:
C      II       I           Input file number
C      NF       I           Number of files
C   Outputs
C      LISTIM   R(3,NF,*)   Time/baseline pair for each record in data
C                           set   1 and 2 are set here.
C      IERR     I           Error
C-----------------------------------------------------------------------
      INTEGER   II, NF, IERR
      REAL      LISTIM(3,NF,*)
C
      INTEGER   LUNIN, INDIN, IBIND, VO, BO, ILENBU, INIO, I, IPT
      LOGICAL   T, F
      CHARACTER FILIN*48
      INCLUDE 'VBGLU.INC'
      REAL      GBUFF(4*LARGER*UVBFSL)
      EQUIVALENCE (GBUFF, BUFFI)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUNIN, T, F /40, .TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('UV', DISKI(II), CNOI(II), 1, FILIN, IERR)
      CALL ZOPEN (LUNIN, INDIN, DISKI(II), FILIN, T, F, F, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN', II
         GO TO 990
         END IF
      ILENBU = 0
      VO = 0
      BO = 1
      JBUFSZ = 2 * 4 * LARGER * UVBFSL
      CALL UVINIT ('READ', LUNIN, INDIN, NVISIN(II), VO, LRECI(II),
     *   ILENBU, JBUFSZ, GBUFF, BO, IBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT', II
         GO TO 990
         END IF
C                                       read
      NUMVIS(II) = 0
      INIO = 1
 100  IF (INIO.GT.0) THEN
         CALL UVDISK ('READ', LUNIN, INDIN, GBUFF, INIO, IBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ', II
            GO TO 990
            END IF
         IPT = IBIND
         DO 120 I = 1,INIO
            NUMVIS(II) = NUMVIS(II) + 1
            LISTIM(1,II,NUMVIS(II)) = GBUFF(IPT+ILOCT)
            IF (ILOCB.GE.0) THEN
               LISTIM(2,II,NUMVIS(II)) = GBUFF(IPT+ILOCB)
            ELSE
               LISTIM(2,II,NUMVIS(II)) = 256.0*GBUFF(IPT+ILOCA1) +
     *            GBUFF(IPT+ILOCA2)
               END IF
            IF ((NUMVIS(II).GT.1) .AND.
     *         (LISTIM(1,II,NUMVIS(II)).LT.LISTIM(1,II,NUMVIS(II)-1)))
     *         SORTED = .FALSE.
            IPT = IPT + LRECI(II)
 120        CONTINUE
         GO TO 100
         END IF
      CALL ZCLOSE (LUNIN, INDIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOS', II
         GO TO 990
         END IF
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I4,2X,A4,'ING FILE',I2)
      END
      SUBROUTINE CNTIMS (SORTED, NV, NF, LISTIM, RECTIM, NVISO)
C-----------------------------------------------------------------------
C   Figure out which output record gets which input records
C   Inputs:
C      NF       I           Number input files
C      LISTIM   R(3,NF,*)   Time/baseline/rec for each record in data
C                           set
C   Outputs
C      RECTIM   I(3,NF,*)   Time/baseline/rec for each record in data
C                           set  - 3rd value set = rec number
C      IERR     I           Error
C-----------------------------------------------------------------------
      LOGICAL   SORTED
      INTEGER   NV(4), NF, RECTIM(3,NF,*), NVISO
      REAL      LISTIM(3,NF,*)
C
      INTEGER   NS(4), I, N, IMIN, NMIN
      REAL      TMIN, BMIN, EPS
      LOGICAL   FIRST
C-----------------------------------------------------------------------
      NVISO = 0
      CALL FILL (4, 1, NS)
      EPS = 0.01 / (24. * 3600.)
C                                       find lowest remaining time
 10   IMIN = 0
      NMIN = 0
      TMIN = 1.E10
      DO 30 N = 1,NF
         FIRST = .TRUE.
         DO 20 I = NS(N),NV(N)
            IF (RECTIM(3,N,I).EQ.0) THEN
               FIRST = .FALSE.
               IF (LISTIM(1,N,I)-TMIN.LT.-EPS) THEN
                  TMIN = LISTIM(1,N,I)
                  BMIN = LISTIM(2,N,I)
                  IMIN = I
                  NMIN = N
               ELSE IF ((SORTED) .AND. (LISTIM(1,N,I)-TMIN.GT.EPS)) THEN
                  GO TO 30
                  END IF
            ELSE
               IF (FIRST) NS(N) = I
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       have one
      IF (IMIN.GT.0) THEN
         NVISO = NVISO + 1
         RECTIM(3,NMIN,IMIN) = NVISO
         IF (IMIN.EQ.NS(NMIN)) NS(NMIN) = NS(NMIN) + 1
C                                       in others?
         DO 50 N = 1,NF
            IF (N.NE.NMIN) THEN
               FIRST = .TRUE.
               DO 40 I = NS(N),NV(N)
                  IF (RECTIM(3,N,I).EQ.0) THEN
                     FIRST = .FALSE.
                     IF ((ABS(LISTIM(1,N,I)-TMIN).LT.EPS) .AND.
     *                  (ABS(LISTIM(2,N,I)-BMIN).LT.0.01)) THEN
                        RECTIM(3,N,I) = NVISO
                        IF (I.EQ.NS(N)) NS(N) = NS(N) + 1
                        GO TO 50
                      ELSE IF ((SORTED) .AND.
     *                  (LISTIM(1,N,I)-TMIN.GT.EPS)) THEN
                        GO TO 50
                        END IF
                  ELSE
                     IF (FIRST) NS(N) = I
                     END IF
 40               CONTINUE
               END IF
 50         CONTINUE
         GO TO 10
         END IF
C
 999  RETURN
      END
      SUBROUTINE SRTIMS (NP, NV, NF, LISTIM, NVISO, LISREC)
C-----------------------------------------------------------------------
C   SRTIMS sorts the list of output record numbers making a list of
C   record numbers to be read
C   Inputs
C      NP       I           Number values in LISTIM (last one counter)
C      NV       I(4)        Number vis in each input file
C      NF       I           Number input files
C      LISTIM   I(3,NF,*)   Lists the output record # in 3 by input #
C      NVISO    I           number output vis
C   Output
C      LISREC   I(NF,*)     Input number for each output
C-----------------------------------------------------------------------
      INTEGER   NP, NV(4), NF, LISTIM(NP,NF,*), NVISO, LISREC(NF,*)
C
      INTEGER   I, N, J
C-----------------------------------------------------------------------
      DO 20 N = 1,NF
         DO 10 I = 1,NV(N)
            J = LISTIM(NP,N,I)
            IF ((J.GT.0) .AND. (J.LE.NVISO)) LISREC(N,J) = I
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE VBGHIS
C-----------------------------------------------------------------------
C   VBGHIS copies and updates history file.  It also copies any tables
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
      CHARACTER NOTTYP(NONOT)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUNO, IERR, I
      INCLUDE 'VBGLU.INC'
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
      CALL HISCOP (LUN1, LUNO, DISKI(1), DISKO, FCNO(2), FCNO(1),
     *   CATBLK, BUFFI, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEI(1), CLASI(1), SEQI(1), DISKI(1),
     *   LUNO, BUFFI, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (NUMFIL.GT.1) THEN
         CALL HENCO2 (TSKNAM, NAMEI(2), CLASI(2), SEQI(2), DISKI(2),
     *      LUNO, BUFFI, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (NUMFIL.GT.2) THEN
         CALL HENCO3 (TSKNAM, NAMEI(3), CLASI(3), SEQI(3), DISKI(3),
     *      LUNO, BUFFI, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (NUMFIL.GT.3) THEN
         CALL HENCO4 (TSKNAM, NAMEI(4), CLASI(4), SEQI(4), DISKI(4),
     *      LUNO, BUFFI, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUNO,
     *   BUFFI, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUNO, HILINE, BUFFI, IERR)
            IF (IERR.NE.0) GO TO 200
 50      CONTINUE
      END IF
C                                       Close HI file
 200  CALL HICLOS (LUNO, .TRUE., BUFFI, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUNO, DISKI(1), DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFFI, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK for output file.
      CALL CATIO ('UPDT', DISKO, FCNO(1), CATBLK, 'REST',
     *   BUFFI, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VBGHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('VBGHIS: ERROR COPYING TABLES')
      END
      SUBROUTINE GETORD (ORDER, OUTIF, INFIL, INIF)
C-----------------------------------------------------------------------
C  Little function that cracks the order code to give the input file
C  and IF that correspond to the new output IF.
C-----------------------------------------------------------------------
      INTEGER ORDER(*), OUTIF, INFIL, INIF
C-----------------------------------------------------------------------
      INFIL = ORDER(OUTIF) / 100
      INIF  = ORDER(OUTIF) - INFIL * 100
      RETURN
      END
      SUBROUTINE GLUDAT (NF, LISREC, JR, INBUFF, IERR)
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
      INTEGER   NF, LISREC(NF,*), JR, IERR
      REAL      INBUFF(JR,*)
C
      INCLUDE 'VBGLU.INC'
      INCLUDE 'CATS.INC'
      INCLUDE 'CONTROL.INC'
      CHARACTER FILIN(4)*48, FILOUT*48, JSORT*4
      INTEGER   LUNIN(4), IN, INDIN(4), ILENBU(4), IBIND(4), OBIND,
     *   VO, BO, INIO(4), NCORI(4), NCORO, NICOPY(4), NOCOPY, IPT, OPT,
     *   I, NIOUT, NIOLIM,OLENBU, LUNOUT, INDOUT, NCNTR(4), CURREC(2,4),
     *   NP, IB(4), OREC, NV, NREINI, VMX, VMN, OLIM, RNXRET
      REAL      RESULT(UVBFSL), TBUFF(UVBFSL), LSTIME, LSBASE, CTIME,
     *   CBASE, RPS(50), TEPS
      LOGICAL   T, F, EOF(4), TSORT, BSORT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNIN, LUNOUT /40, 41, 42, 43, 44/
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      TEPS = 0.01 / 24.0 / 3600.0
      VO = 0
      BO = 1
      LSTIME = -1.E10
      LSBASE = 0.0
      TSORT = .TRUE.
      BSORT = .TRUE.
      JBUFSZ = JR * 2
      EOF(1) = F
      EOF(2) = F
      EOF(3) = F
      EOF(4) = F
      CALL FILL (8, 0, CURREC)
      CALL FILL (4, 0, NCNTR)
      NREINI = 0
C                                       Record sizes in input
C                                       and output files.
      DO 10 IN = 1,NF
         NCORI(IN) = (LRECI(IN) - NRPRMI(IN)) / CATI(KINAX,IN)
         NICOPY(IN) = (LRECI(IN) - NRPRMI(IN)) / NIFI(IN)
         IF (ISCOMP) NICOPY(IN) = NICOPY(IN) * 3
 10      CONTINUE
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NOCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       all input files.
      DO 20 IN = 1,NF
         CALL ZPHFIL ('UV', DISKI(IN), CNOI(IN), 1, FILIN(IN), IERR)
         CALL ZOPEN (LUNIN(IN), INDIN(IN), DISKI(IN), FILIN(IN), T, F,
     *      F, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN', IN
            GO TO 990
            END IF
         ILENBU(IN) = 0
         CALL UVINIT ('READ', LUNIN(IN), INDIN(IN), NVISIN(IN), VO,
     *      LRECI(IN), ILENBU(IN), JBUFSZ, INBUFF(1,IN), BO, IBIND(IN),
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'INIT', IN
            GO TO 990
            END IF
C                                       start reads
         CALL UVDISK ('READ', LUNIN(IN), INDIN(IN), INBUFF(1,IN),
     *      INIO(IN), IBIND(IN), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ', IN
            GO TO 990
            END IF
         CURREC(1,IN) = 1
         CURREC(2,IN) = INIO(IN)
         IF (INIO(IN).LE.0) EOF(IN) = .TRUE.
 20      CONTINUE
C                                       Open the output file
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, FILOUT, IERR)
      CALL ZOPEN (LUNOUT, INDOUT, DISKO, FILOUT, T, F, F, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, 'OPEN'
         GO TO 990
         END IF
      OLENBU = 0
      JBUFSZ = 2 * UVBFSL
      CALL UVINIT ('WRIT', LUNOUT, INDOUT, NVISO, VO, LRECO,
     *   OLENBU, JBUFSZ, BUFFO, BO, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'INIT'
         GO TO 990
         END IF
      OPT = OBIND
      NIOUT = 0
      NIOLIM = OLENBU
C                                       don't use old NX table info
C                                       this sets parms not to
      CALL RNXGET (DISKO, CCNO, CATBLK)
C                                       make an index table
      CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       build output file
      DO 100 OREC = 1,NVISO
C                                       get right buffers
         DO 30 IN = 1,NF
            IF ((LISREC(IN,OREC).LE.0) .OR. (EOF(IN))) THEN
               IB(IN) = 0
            ELSE
C                                       got it already
               IF ((LISREC(IN,OREC).GE.CURREC(1,IN)) .AND.
     *            (LISREC(IN,OREC).LE.CURREC(2,IN))) THEN
                  IB(IN) = (LISREC(IN,OREC) - CURREC(1,IN)) * LRECI(IN)
     *               + IBIND(IN)
C                                       must get it: simple read
               ELSE IF ((LISREC(IN,OREC).GT.CURREC(2,IN)) .AND.
     *            (LISREC(IN,OREC).LE.CURREC(2,IN)+INIO(IN))) THEN
                  CALL UVDISK ('READ', LUNIN(IN), INDIN(IN),
     *               INBUFF(1,IN), INIO(IN), IBIND(IN), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR, 'READ', IN
                     GO TO 990
                     END IF
                  CURREC(1,IN) = CURREC(2,IN) + 1
                  CURREC(2,IN) = CURREC(2,IN) + INIO(IN)
                  IF (INIO(IN).LE.0) EOF(IN) = .TRUE.
                  IB(IN) = (LISREC(IN,OREC) - CURREC(1,IN)) * LRECI(IN)
     *               + IBIND(IN)
C                                       re-init and then read
               ELSE
                  ILENBU(IN) = 0
C                                       try to be smart
                  VMN = LISREC(IN,OREC)
                  VMX = LISREC(IN,OREC)
                  OLIM = NVISO - OREC
                  OLIM = MIN (OLIM, INIO(IN)/2)
                  DO 25 I = 1,OLIM
                     IF (LISREC(IN,OREC+I).GT.0) THEN
                        VMN = MIN (VMN, LISREC(IN,OREC+I))
                        VMX = MAX (VMX, LISREC(IN,OREC+I))
                        END IF
 25                  CONTINUE
                  IF (VMX-VMN+1.LE.INIO(IN)) THEN
                     VO = VMN - 1
                  ELSE
                     VMN = LISREC(IN,OREC)
                     VMX = LISREC(IN,OREC)
                     OLIM = OLIM / 2
                     DO 26 I = 1,OLIM
                        IF (LISREC(IN,OREC+I).GT.0) THEN
                           VMN = MIN (VMN, LISREC(IN,OREC+I))
                           VMX = MAX (VMX, LISREC(IN,OREC+I))
                           END IF
 26                     CONTINUE
                     IF (VMX-VMN+1.LE.INIO(IN)) THEN
                        VO = VMN - 1
                     ELSE
                        VO = LISREC(IN,OREC) - 1
                        END IF
                     END IF
                  NV = NVISIN(IN) - VO
                  NREINI = NREINI + 1
                  JBUFSZ = JR * 2
                  CALL UVINIT ('READ', LUNIN(IN), INDIN(IN), NV, VO,
     *               LRECI(IN), ILENBU(IN), JBUFSZ, INBUFF(1,IN), BO,
     *               IBIND(IN), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR, 'INIT', IN
                     GO TO 990
                     END IF
C                                       start reads
                  CALL UVDISK ('READ', LUNIN(IN), INDIN(IN),
     *               INBUFF(1,IN), INIO(IN), IBIND(IN), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR, 'READ', IN
                     GO TO 990
                     END IF
                  CURREC(1,IN) = VO + 1
                  CURREC(2,IN) = VO + INIO(IN)
                  IF (INIO(IN).LE.0) EOF(IN) = .TRUE.
                  IB(IN) = (LISREC(IN,OREC) - CURREC(1,IN)) * LRECI(IN)
     *               + IBIND(IN)
                  END IF
               END IF
 30         CONTINUE
C                                       build up output record
         NP = 0
         DO 40 IN = 1,NF
            IF (IB(IN).GT.0) THEN
               NP = NP + 1
C                                       random parameters
               IPT = IB(IN)
               IF (NP.EQ.1) THEN
                  CALL RCOPY (NRPRMO, INBUFF(IPT,IN), BUFFO(OPT))
C                                       uvw scaling
                  BUFFO(OPT+ILOCU) = BUFFO(OPT+ILOCU) * UVWSC(IN)
                  BUFFO(OPT+ILOCV) = BUFFO(OPT+ILOCV) * UVWSC(IN)
                  BUFFO(OPT+ILOCW) = BUFFO(OPT+ILOCW) * UVWSC(IN)
                  CTIME = BUFFO(OPT+ILOCT)
                  IF (ILOCB.GE.0) THEN
                     CBASE = BUFFO(OPT+ILOCB)
                  ELSE
                     CBASE = 256.0*BUFFO(OPT+ILOCA1) + BUFFO(OPT+ILOCA2)
                     END IF
                  IF (CTIME-LSTIME.GT.TEPS) THEN
                     LSTIME = CTIME
                     LSBASE = 0
                     END IF
                  IF (CTIME.LT.LSTIME-TEPS) TSORT = .FALSE.
                  IF (CBASE.LT.LSBASE) BSORT = .FALSE.
                  LSBASE = CBASE
               ELSE
                  CALL RCOPY (NRPRMO, INBUFF(IPT,IN), RPS)
                  IF (ILOCB.GE.0) THEN
                     IF ((ABS(RPS(1+ILOCT)-BUFFO(OPT+ILOCT)).GT.TEPS)
     *                  .OR.
     *                  (ABS(RPS(1+ILOCB)-BUFFO(OPT+ILOCB)).GT.0.01))
     *                  THEN
                        WRITE (MSGTXT,1030) IN, OREC
                        CALL MSGWRT (8)
                        END IF
                  ELSE
                     IF ((ABS(RPS(1+ILOCT)-BUFFO(OPT+ILOCT)).GT.TEPS)
     *                  .OR.
     *                  (ABS(RPS(1+ILOCA1)-BUFFO(OPT+ILOCA1)).GT.0.01)
     *                  .OR.
     *                  (ABS(RPS(1+ILOCA2)-BUFFO(OPT+ILOCA2)).GT.0.01))
     *                  THEN
                        WRITE (MSGTXT,1030) IN, OREC
                        CALL MSGWRT (8)
                        END IF
                     END IF
                  END IF
C                                       Compressed
               IF (ISCOMP) THEN
                  CALL ZUVXPN (NCORI(IN), INBUFF((IPT+NRPRMI(IN)),IN),
     *               INBUFF((IPT+ILOCWT),IN), TBUFF)
                  CALL LOADIF (NP, ORDER, NIFO, IN, NICOPY(IN),
     *               TBUFF, RESULT)
C                                       Uncompressed
               ELSE
                  CALL LOADIF (NP, ORDER, NIFO, IN, NICOPY(IN),
     *               INBUFF((IPT+NRPRMI(IN)),IN), BUFFO(OPT+NRPRMO))
                  END IF
               END IF
 40         CONTINUE
         IF (ISCOMP) CALL ZUVPAK (NCORO, RESULT, BUFFO(OPT+ILOCWT),
     *      BUFFO(OPT+NRPRMO))
C                                       update NX table
         CALL RNXUPD (BUFFO(OPT), RNXRET)
         NCNTR(NP) = NCNTR(NP) + 1
C                                       update pointers
         OPT = OPT + LRECO
         NIOUT = NIOUT + 1
C                                       Write if buffer full
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNOUT, INDOUT, BUFFO, NIOLIM, OBIND,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR, 'WRIT'
               GO TO 990
               END IF
            OPT = OBIND
            NIOUT = 0
            END IF
 100     CONTINUE
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNOUT, INDOUT, BUFFO, NIOUT, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'FLSH'
         GO TO 990
         END IF
C                                       sort order
      JSORT = ' '
      IF (TSORT) THEN
         JSORT = 'T'
         IF (BSORT) JSORT(2:) = 'B'
         END IF
      CALL CHR2H (4, JSORT, 1, CATBLK(KITYP))
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
      DO 810 IN = 1,NF
         CALL ZCLOSE (LUNIN(IN), INDIN(IN), IERR)
 810     CONTINUE
      DO 820 I = 1,NF
         WRITE (MSGTXT,1810) NCNTR(I), I
         CALL MSGWRT (4)
 820     CONTINUE
      WRITE (MSGTXT,1820) NREINI
      CALL MSGWRT (4)
      IERR = 0
      GO TO 999
C                                       Error
  990 CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GLUDAT: ERROR',I3,1X,A4,'ING FILE ',I3)
 1010 FORMAT ('GLUDAT: ERROR',I3,1X,A4,'ING OUTPUT FILE')
 1030 FORMAT ('GLUDAT: OUT RECORD',I10,' IN FILE',I2,' TIME/BASELINE',
     *   ' NOT MATCH!')
 1810 FORMAT ('Wrote ',I8,' visibilities with data merged in from',I2,
     *   ' files')
 1820 FORMAT ('Operation required',I8,' I/O re-initializations')
      END
      SUBROUTINE GLUTAB (IERR)
C-----------------------------------------------------------------------
C   Routine that performs the bonding process for tables. Since there
C   are so many tables that need operating on this will do it in a
C   generic sense.
C   Output:
C      IERR   I   Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'VBGLU.INC'
      INCLUDE 'CATS.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'TIMLST.INC'
      INTEGER   TABDO, MAXIFV, MAXKEY, MAXTWO, MAXIDC
      PARAMETER (TABDO = 18, MAXIFV = 16, MAXKEY = 50)
      PARAMETER (MAXTWO = 10, MAXIDC = 6)
      CHARACTER TABGLU(TABDO)*2, TABIFS(MAXIFV,TABDO)*24,
     *   IFKEY(TABDO)*8, KEYWRD(MAXKEY)*8, TWODIM(MAXTWO,TABDO)*24,
     *   IDKOLS(MAXIDC,TABDO)*24, TABIF1(MAXIFV,6)*24,
     *   TABIF2(MAXIFV,8)*24, TABIF3(MAXIFV,TABDO-14)*24,
     *   IDKOL1(MAXIDC,6)*24, IDKOL2(MAXIDC,6)*24,
     *   IDKOL3(MAXIDC,TABDO-12)*24
      LONGINT   LL
      INTEGER   II, NTAB(4), VER, LUNIN(4), LUNOUT, NKEY, NREC, NCOL,
     *   DATP(256,4), NROWS(4), COLKEY(TABDO,2), UNIQUE(TABDO), J, NP,
     *   LOGCOL(MAXIFV,4), IN, BUFFER(512,4), BUFOUT(512), IROW,
     *   RECI(XBPRSZ), RECO(XBPRSZ), DATPO(256), ICOL, LENGTH, RTYPE,
     *   IPTR, OPTR, ITEMP(6), KLOCS(MAXKEY), KVALS(MAXKEY), NROWO,
     *   KTYP(MAXKEY), DIMT(TABDO), TWOCOL(MAXTWO,4), TWOKEY(TABDO,2),
     *   TWOLEN, IDKEY(TABDO), IDUNIQ(TABDO), IDCOL(MAXIDC,4), IDUM,
     *   NKY, NCNTR(4), ACOLK, ATWOK, ICTRNO, CTKOLS(12), CTNUMV(12),
     *   I, INUM, NR, NW, LVER, NVER
      REAL      RECRI(XBPRSZ), RECRO(XBPRSZ), RTEMP
      DOUBLE PRECISION RECDI(XBPRSZ/2), RECDO(XBPRSZ/2)
      LOGICAL   WANKOL, TWOD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INTEGER   MAXFLG, SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2),
     *   LIFS(2), LFGRNO
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
      CALL FILL (4, 0, LUNIN)
C                                       Loop over file types
      DO 100 II = 1,TABDO
         NVER = 0
         DO 10 IN = 1,NUMFIL
            CALL FNDEXT (TABGLU(II), CATI(1,IN), NTAB(IN))
            NVER = MAX (NVER, NTAB(IN))
 10         CONTINUE
         IF (NVER.LE.0) GO TO 100
         IF (TABGLU(II).EQ.'CT') THEN
            CALL CTTAB (IERR)
            IF (IERR.GT.0) THEN
               MSGTXT = 'GLUTAB: CT tables not copied'
               CALL MSGWRT (6)
               GO TO 100
               END IF
            LUNOUT = 40
            LUNIN(1) = 40 + 1
C                                        Copy CT table
            CALL TABCOP ('CT', 1, 1, LUNIN(1), LUNOUT, DISKI(1), DISKO,
     *         CNOI(1), CCNO, CATBLK, BUFFER(1,1), BUFOUT, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'TABCOP', 'GLUTAB', IERR)
               LUNIN(IN) = 0
               GO TO 100
               END IF
C                                        Update number of IFs
            CALL CTINI ('WRIT', BUFOUT, DISKO, CCNO, 1, CATBLK, LUNOUT,
     *         ICTRNO, CTKOLS, CTNUMV, IERR)
            CALL UPDKEY (BUFOUT, IFKEY(II), 4, NIFO, IERR)
            IF (IERR.NE.0) CALL TABERR ('UPDT', 'UPDKEY', 'GLUTAB',
     *         IERR)
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
C                                       Loop over version
         DO 99 LVER = 1,NVER
            WRITE (MSGTXT,1019) TABGLU(II), LVER
            CALL MSGWRT (2)
            CALL FILL (4, 0, NCNTR)
            OFFSET = 0
            DFFSET = 0
            POFSET = 0
            TWOD = .FALSE.
            VER = LVER
            NP = 0
            NWORDS = 0
            DO 20 IN = 1,NUMFIL
               IF (NTAB(IN).LT.LVER) THEN
                  NROWS(IN) = 0
               ELSE
                  LUNIN(IN) = 40 + IN
                  CALL TABINI ('READ', TABGLU(II), DISKI(IN), CNOI(IN),
     *               VER, CATI(1,IN), LUNIN(IN), NKEY, NREC, NCOL,
     *               DATP(1,IN), BUFFER(1,IN), IERR)
                  IF (IERR.NE.0) THEN
                     CALL TABERR ('READ', 'TABINI', 'GLUTAB', IERR)
                     LUNIN(IN) = 0
                     GO TO 90
                     END IF
C                                       # rows in table
                  NROWS(IN) = BUFFER(5,IN)
                  NWORDS = MAX (NWORDS, NROWS(IN))
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
                     CALL TABKEY ('READ', 'NOPCAL  ', NKY, BUFFER(1,IN),
     *                  KLOCS, KVALS, KTYP, IERR)
                  ELSE IF (TABGLU(II).EQ.'IM') THEN
                     CALL TABKEY ('READ', 'NPOLY   ', NKY, BUFFER(1,IN),
     *                  KLOCS, KVALS, KTYP, IERR)
                  ELSE IF (TABGLU(II).EQ.'PC') THEN
                     CALL TABKEY ('READ', 'NO_TONES', NKY, BUFFER(1,IN),
     *                  KLOCS, KVALS, KTYP, IERR)
                  ELSE IF (TABGLU(II).EQ.'BP') THEN
                     CALL TABKEY ('READ', 'NO_CHAN ', NKY, BUFFER(1,IN),
     *                  KLOCS, KVALS, KTYP, IERR)
                  ELSE IF (TABGLU(II).EQ.'PD') THEN
                     CALL TABKEY ('READ', 'NO_CHAN ', NKY, BUFFER(1,IN),
     *                  KLOCS, KVALS, KTYP, IERR)
                  ELSE IF (TABGLU(II).EQ.'CP') THEN
                     CALL TABKEY ('READ', 'NO_CHAN ', NKY, BUFFER(1,IN),
     *                  KLOCS, KVALS, KTYP, IERR)
                  ELSE IF (TABGLU(II).EQ.'GC') THEN
                     CALL TABKEY ('READ', 'NO_TABS ', NKY, BUFFER(1,IN),
     *                  KLOCS, KVALS, KTYP, IERR)
                  ELSE IF (TABGLU(II).EQ.'AN') THEN
                     CALL TABKEY ('READ', 'NOPCAL  ', NKY, BUFFER(1,IN),
     *                  KLOCS, KVALS, KTYP, IERR)
C                                       8-char column
C                                       this forces it to be 2-D
                  ELSE IF (TABGLU(II).EQ.'CQ') THEN
                     TWOD = .TRUE.
                     DIMT(II) = 8
                     END IF
                  IF (IERR.GT.0) THEN
                     CALL TABERR ('READ', 'TABKEY', 'GLUTAB', IERR)
                     GO TO 90
                     END IF
                  IF (NP.LE.1) THEN
                     IF ((KLOCS(1).GT.0) .AND. (KTYP(1).EQ.4))
     *                  DIMT(II) = KVALS(KLOCS(1))
C                                       ???
C                    IF (DIMT(II).EQ.0) DIMT(II) = 1
                     IF (DIMT(II).GT.1) TWOD = .TRUE.
                  ELSE IF ((KLOCS(1).GT.0) .AND. (KTYP(1).EQ.4)) THEN
                     IF (DIMT(II).NE.KVALS(KLOCS(1))) THEN
                        WRITE (MSGTXT,1010) TABGLU(II), DIMT(II),
     *                     KVALS(KLOCS(1))
                        CALL MSGWRT (8)
                        GO TO 90
                     END IF
                     IF (KVALS(KLOCS(1)).EQ.0) KVALS(KLOCS(1)) = 1
                     END IF
C                                       Find columns for row
C                                       recognition, these are same
C                                       for input & output tables.
                  CALL FILL (MAXIDC, 0, IDCOL(1,IN))
                  CALL FNDCOL (IDKEY(II), IDKOLS(1,II), IDUNIQ(II),
     *               .TRUE., BUFFER(1,IN), IDCOL(1,IN), IERR)
                  IF (IERR.NE.0) THEN
                     IF ((IERR.NE.11) .OR. (TABGLU(II).NE.'AT')) THEN
                        WRITE (MSGTXT,1011) TABGLU(II), 'ID', IN
                        CALL MSGWRT (8)
                        GO TO 90
C                                       no source column in AT table
                     ELSE
                        IERR = 0
                        END IF
                     END IF
C                                       Find columns
                  CALL FILL (MAXIFV, 0, LOGCOL(1,IN))
                  CALL FNDCOL (ACOLK, TABIFS(1,II), UNIQUE(II),.TRUE.,
     *               BUFFER(1,IN), LOGCOL(1,IN), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1011) TABGLU(II), 'IF', IN
                     CALL MSGWRT (8)
                     GO TO 90
                     END IF
C                                       Find 2-D columns
                  CALL FILL (MAXTWO, 0, TWOCOL(1,IN))
                  IF (TWOD) THEN
                     CALL FNDCOL (ATWOK, TWODIM(1,II), UNIQUE(II),
     *                  .TRUE., BUFFER(1,IN), TWOCOL(1,IN), IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,1011) TABGLU(II), '2D', IN
                        CALL MSGWRT (8)
                        GO TO 90
                        END IF
                     END IF
C                                       Close to tidy up after FNDCOL
                  IDUM = 0
                  CALL TABIO ('CLOS', IDUM, NROWS(IN), BUFFER(1,IN),
     *            BUFFER(1,IN), IERR)
C                                       Reopen for read
                  VER = LVER
                  CALL TABINI ('READ', TABGLU(II), DISKI(IN), CNOI(IN),
     *               VER, CATI(1,IN), LUNIN(IN), NKEY, NREC, NCOL,
     *               DATP(1,IN), BUFFER(1,IN), IERR)
                  IF (IERR.NE.0) THEN
                     CALL TABERR ('READ', 'TABINI', 'GLUTAB', IERR)
                     LUNIN(IN) = 0
                     GO TO 90
                     END IF
                  END IF
 20            CONTINUE
            IF (NWORDS.LE.0) THEN
               WRITE (MSGTXT,1020) TABGLU(II), LVER
               CALL MSGWRT (8)
               GO TO 90
               END IF
C                                       get memory
            NWORDS = (NUMFIL * 8 * NWORDS - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, LISTIM, OFFSET, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'FAILED TO GET DYNAMIC MEMORY' //
     *            ' FOR TABLE TIME LIST'
               CALL MSGWRT (8)
               GO TO 90
               END IF
            DFFSET = (OFFSET+1)/2
            CALL FILL (1024*NWORDS, 0, LISTIM(1+OFFSET))
C                                       fill list of times/IDs
            DO 30 IN = 1,NUMFIL
               CALL TATIM1 (TABGLU(II), IN, NUMFIL, IDCOL(1,IN),
     *            NROWS(IN), DLISTM(1+DFFSET), RLISTM(1+OFFSET),
     *            LISTIM(1+OFFSET), DATP(1,IN), BUFFER(1,IN), IERR)
               IF (IERR.NE.0) GO TO 90
 30            CONTINUE
            CALL TATIMS (NROWS, NUMFIL, DLISTM(1+DFFSET),
     *         RLISTM(1+OFFSET),LISTIM(1+OFFSET), NROWO)
C                                       sort
            PWORDS = (NUMFIL * NROWO - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', TSKNAM, PWORDS, LISREC, POFSET, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'FAILED TO GET DYNAMIC MEMORY FOR SORTED LIST'
               CALL MSGWRT (8)
               GO TO 90
               END IF
            CALL FILL (1024*PWORDS, 0, LISREC(1+POFSET))
            CALL SRTIMS (8, NROWS, NUMFIL, LISTIM(1+OFFSET), NROWO,
     *         LISREC(1+POFSET))
C                                       select master input file
            IN = 0
            J = 0
            DO 35 NP = 1,NUMFIL
               IF (NROWS(NP).GT.J) THEN
                  J = NROWS(NP)
                  IN = NP
                  END IF
 35            CONTINUE
            IF (IN.EQ.0) GO TO 90
C                                       Open output table for write
C                                       Update DATP array first
            CALL FILL (256, 0, DATPO)
            CALL COPY (128, DATP(129,IN), DATPO(129))
            DO 40 ICOL = 1,NCOL
               IF (WANKOL(ICOL,ACOLK,LOGCOL(1,IN))) THEN
                  LENGTH = DATPO(128+ICOL) / 10
                  RTYPE = DATPO(128+ICOL) - LENGTH * 10
                  LENGTH = LENGTH * NIFO / NIFI(IN)
                  DATPO(128+ICOL) = RTYPE + LENGTH * 10
                  END IF
 40            CONTINUE
C                                       create output table
            VER = LVER
            NREC = 30
            LUNOUT = 40
            CALL TABINI ('WRIT', TABGLU(II), DISKO, CCNO, VER, CATBLK,
     *         LUNOUT, NKEY, NREC, NCOL, DATPO, BUFOUT, IERR)
            IF (IERR.NE.-1) THEN
               CALL TABERR ('WRIT', 'TABINI', 'GLUTAB', IERR)
               LUNOUT = 0
               GO TO 90
               END IF
C                                       Copy keyword/value pairs
            CALL TABKEY ('ALL ', KEYWRD, NKEY, BUFFER(1,IN), KLOCS,
     *         KVALS, KTYP, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('ALL ', 'TABKEY', 'GLUTAB', IERR)
               GO TO 90
               END IF
            CALL TABKEY ('WRIT', KEYWRD, NKEY, BUFOUT, KLOCS, KVALS,
     *         KTYP, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABKEY', 'GLUTAB', IERR)
               GO TO 90
               END IF
C                                       Copy col labels
            DO 45 ICOL = 1,NCOL
               IDUM = 3
               CALL TABIO ('READ', IDUM, ICOL, ITEMP, BUFFER(1,IN),
     *            IERR)
               CALL TABIO ('WRIT', IDUM, ICOL, ITEMP, BUFOUT, IERR)
               IDUM = 4
               CALL TABIO ('READ', IDUM, ICOL, ITEMP, BUFFER(1,IN),
     *            IERR)
               CALL TABIO ('WRIT', IDUM, ICOL, ITEMP, BUFOUT, IERR)
 45            CONTINUE
C                                       Update catalogue header
            CALL CATIO ('UPDT', DISKO, CCNO, CATBLK, 'REST', BUFFI,
     *         IERR)
C                                       Update IF keyword
            CALL UPDKEY (BUFOUT, IFKEY(II), 4, NIFO, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('UPDT', 'UPDKEY', 'GLUTAB', IERR)
               GO TO 90
               END IF
C                                       Now loop through the rows
C                                       reading and enlarging IF
C                                       dependent ones. Placing
C                                       values from incoming tables in
C                                       the apropriate places in the
C                                       outgoing table.
            DO 70 IROW = 1,NROWO
               NP = 0
               LL = POFSET + NUMFIL * (IROW - 1)
               DO 60 IN = 1,NUMFIL
                  LL = LL + 1
                  J = LISREC(LL)
                  IF (J.GT.0) THEN
                     NP = NP + 1
                     IDUM = 0
                     CALL TABIO ('READ', IDUM, J, RECI, BUFFER(1,IN),
     *                  IERR)
                     IF (IERR.NE.0) THEN
                        CALL TABERR ('READ', 'TABIO ', 'GLUTAB', IERR)
                        GO TO 90
                        END IF
C                                       Run through columns
                     DO 50 ICOL = 1,NCOL
C                                       get type, length
                        LENGTH = DATP(128+ICOL,IN) / 10
                        RTYPE = DATP(128+ICOL,IN) - LENGTH * 10
                        IF (LENGTH.GT.0) THEN
                           IPTR = DATP(ICOL,IN)
                           OPTR = DATPO(ICOL)
C                                       bad  type
                           IF ((RTYPE.LT.1) .OR. (RTYPE.GT.7)) THEN
                              WRITE (MSGTXT,1045) TABGLU(II), IROW,
     *                           ICOL, RTYPE
                              CALL MSGWRT (8)
                              IERR = 5
                              GO TO 90
                              END IF
C                                       Straight copy
                           IF (.NOT.WANKOL(ICOL, ACOLK, LOGCOL(1,IN)))
     *                        THEN
C                                       Do only once
                              IF (NP.EQ.1) THEN
                                 IF (RTYPE.EQ.1) THEN
                                    CALL DPCOPY (LENGTH, RECDI(IPTR),
     *                                 RECDO(OPTR))
                                 ELSE IF (RTYPE.EQ.2) THEN
                                    CALL RCOPY (LENGTH, RECRI(IPTR),
     *                                 RECRO(OPTR))
                                 ELSE IF (RTYPE.EQ.3) THEN
                                    I = (LENGTH + 3) / 4
                                    CALL RCOPY (I, RECRI(IPTR),
     *                                 RECRO(OPTR))
                                 ELSE IF (RTYPE.GE.4) THEN
                                    CALL COPY (LENGTH, RECI(IPTR),
     *                                 RECO(OPTR))
                                    END IF
                                 END IF
C                                       Reshuffle order
                           ELSE
                              TWOLEN = 1
                              IF (WANKOL(ICOL, ATWOK, TWOCOL(1,IN)))
     *                           TWOLEN = DIMT(II)
C                                IF (TWOLEN*NIFI(IN).LT.LENGTH)
C    *                              TWOLEN = LENGTH / NIFI(IN)
C                                END IF
                              CALL LOADTB (NP, RTYPE, ORDER, NIFO, IN,
     *                           TWOLEN, RECI(IPTR), RECO(OPTR),
     *                           RECRI(IPTR), RECRO(OPTR),
     *                           RECDI(IPTR), RECDO(OPTR))
                              END IF
                           END IF
 50                     CONTINUE
                     END IF
 60               CONTINUE
C                                       Write output record
               IDUM = 0
               NCNTR(NP) = NCNTR(NP) + 1
               CALL TABIO ('WRIT', IDUM, IROW, RECO, BUFOUT, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABERR ('WRIT', 'TABIO ', 'GLUTAB', IERR)
                  GO TO 999
                  END IF
 70           CONTINUE
C                                       Close 'em down
 90         IDUM = 0
            DO 95 IN = 1,NUMFIL
               IF (LUNIN(IN).GT.0) CALL TABIO ('CLOS', IDUM, NROWS(IN),
     *            BUFFER(1,IN), BUFFER(1,IN), IERR)
               LUNIN(IN) = 0
 95            CONTINUE
            IF (LUNOUT.GT.0) CALL TABIO ('CLOS', IDUM, NROWO, BUFOUT,
     *         BUFOUT, IERR)
            LUNOUT = 0
            RTEMP = 0.0
            DO 96 NP = 1,NUMFIL
               IF (NP.LT.NUMFIL) RTEMP = RTEMP + NCNTR(NP)
               WRITE (MSGTXT,1095) TABGLU(II), LVER, NCNTR(NP), NP
               IF (NCNTR(NP).GT.0) CALL MSGWRT (4)
 96            CONTINUE
            IF (NCNTR(NUMFIL).EQ.0) NCNTR(NUMFIL) = 1
            IF (RTEMP/NCNTR(NUMFIL).GT.0.1) THEN
               MSGTXT = '    THIS SUGGESTS THAT THESE ' // TABGLU(II) //
     *            ' TABLES WERE NOT WELL MATCHED'
               CALL MSGWRT (7)
               END IF
            IF (OFFSET.NE.0) CALL ZMEMRY ('FREE', TSKNAM, NWORDS,
     *         LISTIM, OFFSET, IERR)
            IF (POFSET.NE.0) CALL ZMEMRY ('FREE', TSKNAM, PWORDS,
     *         LISREC, POFSET, IERR)
 99         CONTINUE
 100     CONTINUE
C                                       Do FG tables if any
      MSGTXT = 'Copy/reformat flag tables if any'
      CALL MSGWRT (2)
      DO 200 IN = 1,NUMFIL
         CALL FNDEXT ('FG', CATI(1,IN), NTAB(IN))
         IF (NTAB(IN).GE.1) THEN
            IF (NTAB(IN).GT.1) THEN
               WRITE (MSGTXT,1100) IN, NTAB(IN)
               CALL MSGWRT (6)
               END IF
            LUNIN(IN) = 40 + IN
            CALL FLGINI ('READ', BUFFER(1,IN), DISKI(IN), CNOI(IN),
     *         NTAB(IN), CATI(1,IN), LUNIN(IN), IFGRNO, FGKOLS,
     *         FGNUMV, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'FLGINI', 'GLUTAB', IERR)
               GO TO 200
               END IF
            VER = 1
            LUNOUT = 40
            CALL FLGINI ('WRIT', BUFOUT, DISKO, CCNO, VER, CATBLK,
     *         LUNOUT, LFGRNO, FGKOLS, FGNUMV, IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('WRIT', 'FLGINI', 'GLUTAB', IERR)
               GO TO 195
               END IF
            NROWS(IN) = BUFFER(5,IN)
            NR = 0
            NW = 0
            DO 150 IROW = 1,NROWS(IN)
               IFGRNO = IROW
               CALL TABFLG ('READ', BUFFER(1,IN), IFGRNO, FGKOLS,
     *            FGNUMV, SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS,
     *            PFLAGS, REASON, IERR)
               IF (IERR.GT.0) THEN
                  CALL TABERR ('READ', 'TABFLG', 'GLUTAB', IERR)
                  GO TO 195
               ELSE IF (IERR.EQ.0) THEN
                  NR = NR + 1
                  IF (IFS(1).LE.0) IFS(1) = 1
                  IF (IFS(2).LT.IFS(1)) IFS(2) = MAXIF
                  DO 110 I = 1,NIFO
                     CALL GETORD (ORDER, I, INUM, J)
                     IF ((INUM.EQ.IN) .AND. (J.GE.IFS(1)) .AND.
     *                  (J.LE.IFS(2))) THEN
                        LIFS(1) = I
                        LIFS(2) = I
                        NW = NW + 1
                        CALL TABFLG ('WRIT', BUFOUT, LFGRNO, FGKOLS,
     *                     FGNUMV, SOURID, SUBA, FREQID, ANTS, TIMER,
     *                     LIFS, CHANS, PFLAGS, REASON,IERR)
                        IF (IERR.NE.0) THEN
                           CALL TABERR ('WRIT', 'TABFLG', 'GLUTAB',
     *                        IERR)
                           GO TO 195
                           END IF
                        END IF
 110                 CONTINUE
                  END IF
 150           CONTINUE
            WRITE (MSGTXT,1150) IN, NR, NW
            CALL MSGWRT (4)
C                                       close out
            CALL TABFLG ('CLOS', BUFOUT, LFGRNO, FGKOLS, FGNUMV, SOURID,
     *         SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *         IERR)
C                                       close in
 195        CALL TABFLG ('CLOS', BUFFER(1,IN), IFGRNO, FGKOLS, FGNUMV,
     *         SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *         REASON, IERR)
            END IF
 200     CONTINUE
      IERR = 0
C
  999 RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('GLUTAB TABLE: ',A2,' COLUMN DIMENSIONS DO NOT MATCH',
     *   2I4)
 1011 FORMAT ('GLUTAB TABLE: ',A2,' MISSING REQUIRED ',A,
     *   ' COLUMNS, FILE',I2)
 1019 FORMAT ('GLUTAB processing table ',A2,' version',I4)
 1020 FORMAT ('GLUTAB TABLE ',A2,' VERS',I3,' NO ROWS TO COPY')
 1045 FORMAT ('GLUTAB: TABLE ',A2,' ROW ',I6,' COL ',I2,' HAS ILLEGAL',
     *   ' TYPE ',I3)
 1095 FORMAT (A2,' table vers',I3,' copied:',I7,' rows from',I2,
     *   ' input files')
 1100 FORMAT ('GLUTAB: File',I2,' has',I3,' FG tables, will only',
     *   ' copy last')
 1150 FORMAT ('GLUTAB: FG file for',I2,' read',I7,' rows, wrote',I7,
     *   ' rows')
      END
      SUBROUTINE TATIM1 (TABGLU, II, NF, IDCOL, NROWS, TIMES, DTIMES,
     *   DSELS, DATP, BUFFER, IERR)
C-----------------------------------------------------------------------
C   Reads in the data selection parameters from a table
C   Inputs:
C      TABGLU   C*2         Table type
C      II       I           Input file number
C      NF       I           Number of files
C      IDCOL    I(*)        Column pointers
C      NROWS    I           Number of rows to read
C   In/Out:
C      BUFFER   I(512)      Open table buffer
C   Outputs:
C      TIMES    D(4,NF,*)   Times - subs 1
C      DTIMES   R(8,NF,*)   Delta times - subs 3
C      DSELS    I(8,NF,*)   Source, antenna, subarray, FQ - subs 4-7
C                           subs 8 = IERR of TABIO
C      IERR     I           Error code
C-----------------------------------------------------------------------
      CHARACTER TABGLU*2
      INTEGER   II, NF, IDCOL(*), NROWS, DSELS(8,NF,*), DATP(256),
     *   BUFFER(512), IERR
      DOUBLE PRECISION TIMES(4,NF,*)
      REAL      DTIMES(8,NF,*)
C
      INTEGER   IDUM, IR
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   RECI(XBPRSZ)
      REAL      RECRI(XBPRSZ)
      DOUBLE PRECISION RECDI(XBPRSZ/2)
      EQUIVALENCE (RECI, RECRI, RECDI)
C-----------------------------------------------------------------------
C                                       loop over input rows for info
      IDUM = 0
      DO 100 IR = 1,NROWS
         CALL TABIO ('READ', IDUM, IR, RECI, BUFFER, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('READ', 'TABIO', 'TATIM1', IERR)
            GO TO 999
            END IF
C                                       Get variables
         TIMES(1,II,IR) = 0.D0
         DTIMES(3,II,IR) = 0.0
         DSELS(4,II,IR) = 0
         DSELS(5,II,IR) = 0
         DSELS(6,II,IR) = 0
         DSELS(7,II,IR) = 0
         DSELS(8,II,IR) = IERR
C                                       special cases
         IF (TABGLU.EQ.'SU') THEN
            DSELS(4,II,IR) = RECI(DATP(IDCOL(1)))
         ELSE IF (TABGLU.EQ.'GC') THEN
            DSELS(5,II,IR) = RECI(DATP(IDCOL(1)))
            DSELS(6,II,IR) = RECI(DATP(IDCOL(2)))
            DSELS(7,II,IR) = RECI(DATP(IDCOL(3)))
         ELSE IF (TABGLU.EQ.'CQ') THEN
            DSELS(7,II,IR) = RECI(DATP(IDCOL(1)))
            DSELS(6,II,IR) = RECI(DATP(IDCOL(2)))
         ELSE IF (TABGLU.EQ.'AN') THEN
            DSELS(5,II,IR) = RECI(DATP(IDCOL(1)))
         ELSE IF (TABGLU.EQ.'CD') THEN
            DSELS(5,II,IR) = RECI(DATP(IDCOL(1)))
            DSELS(6,II,IR) = RECI(DATP(IDCOL(2)))
            DSELS(7,II,IR) = RECI(DATP(IDCOL(3)))
         ELSE IF (TABGLU.EQ.'PD') THEN
            DSELS(5,II,IR) = RECI(DATP(IDCOL(1)))
            DSELS(6,II,IR) = RECI(DATP(IDCOL(2)))
            DSELS(7,II,IR) = RECI(DATP(IDCOL(3)))
         ELSE IF (TABGLU.EQ.'CP') THEN
            DSELS(4,II,IR) = RECI(DATP(IDCOL(2)))
         ELSE IF (TABGLU.EQ.'CT') THEN
C                                       tables with everything
         ELSE
            CALL GETVAR (DATP, IDCOL, RECI, RECRI, RECDI,
     *         TIMES(1,II,IR), DTIMES(3,II,IR), DSELS(4,II,IR),
     *         DSELS(5,II,IR), DSELS(6,II,IR), DSELS(7,II,IR))
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TATIMS (NROWS, NF, TIMES, DTIMES, DSELS, NROWO)
C-----------------------------------------------------------------------
C   figure out which output row gets which input rows
C   Inputs:
C      NF       I           Number of files
C      NROWS    I(*)        Number of rows to read
C      TIMES    D(4,NF,*)   Times - subs 1
C      DTIMES   R(8,NF,*)   Delta times - subs 3
C   In/Out
C      DSELS    I(8,NF,*)   Source, antenna, subarray, FQ - subs 4-7
C                           subs 8 = IERR of TABIO
C                           out if > 0 is output row
C   Output:
C      NROWO    I           Number output rows
C      IERR     I           Error code
C------------------------------------------------------
      INTEGER   NF, NROWS(*), DSELS(8,NF,*), NROWO
      DOUBLE PRECISION TIMES(4,NF,*)
      REAL      DTIMES(8,NF,*)
C
      INTEGER   NS(4), I, N, IMIN, NMIN
      REAL      EPS
      DOUBLE PRECISION TMIN
C-----------------------------------------------------------------------
      NROWO = 0
      CALL FILL (4, 1, NS)
C                                       find lowest remaining time
 10   IMIN = 0
      NMIN = 0
      TMIN = 1.D10
      EPS = -0.01 / (24. * 3600.)
      DO 30 N = 1,NF
         DO 20 I = NS(N),NROWS(N)
            IF (DSELS(8,N,I).EQ.0) THEN
               IF (TIMES(1,N,I)-TMIN.LT.EPS) THEN
                  TMIN = TIMES(1,N,I)
                  IMIN = I
                  NMIN = N
                  END IF
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       have one
      IF (IMIN.GT.0) THEN
         NROWO = NROWO + 1
         DSELS(8,NMIN,IMIN) = NROWO
         IF (IMIN.EQ.NS(NMIN)) NS(NMIN) = NS(NMIN) + 1
         EPS = 0.01 / (24. * 3600.)
         EPS = MAX (EPS, 0.01*DTIMES(3,NMIN,IMIN))
C                                       in others?
         DO 50 N = 1,NF
            IF (N.NE.NMIN) THEN
               DO 40 I = NS(N),NROWS(N)
                  IF (DSELS(8,N,I).EQ.0) THEN
                     IF ((ABS(TIMES(1,N,I)-TMIN).LT.EPS) .AND.
     *                  (DSELS(4,N,I).EQ.DSELS(4,NMIN,IMIN)) .AND.
     *                  (DSELS(5,N,I).EQ.DSELS(5,NMIN,IMIN)) .AND.
     *                  (DSELS(6,N,I).EQ.DSELS(6,NMIN,IMIN)) .AND.
     *                  (DSELS(7,N,I).EQ.DSELS(7,NMIN,IMIN))) THEN
                        DSELS(8,N,I) = NROWO
                        IF (I.EQ.NS(N)) NS(N) = NS(N) + 1
                        GO TO 50
                        END IF
                     END IF
 40               CONTINUE
               END IF
 50         CONTINUE
         GO TO 10
         END IF
C
 999  RETURN
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
      CALL TABKEY ('WRIT', KEYWRD, KEYNUM, BUFFER, LOCS,
     *   KEYVAL, KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C
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
      SUBROUTINE LOADIF (FPASS, ORDER, NIFO, INFIL, IFLEN, BUFFIN,
     *   BUFOUT)
C-----------------------------------------------------------------------
C  Routine that actually loads up the output buffer using the IF ORDER
C  defined.
C   Inputs:
C     FPASS      i          If 1 is the first pass through the data
C                           so must ensure that all unused space in
C                           BUFOUT is 0. If > 1, no zeroing.
C     ORDER      I(*)       Order of IFs
C     NIFO       I          # IFs in output file
C     INFIL      I          File number of input data stream
C     BUFFIN     R(*)       Buffer containing input data stream
C     IFLEN      I          The length of the data segement for a
C                           given IF.
C   Outputs:
C     BUFOUT     R(*)       Buffer containing output data stream
C-----------------------------------------------------------------------
      INTEGER   FPASS, ORDER(*), NIFO, INFIL, IFLEN
      REAL      BUFFIN(*), BUFOUT(*)
C
      INTEGER   I, J, INUM, START, STOP
C-----------------------------------------------------------------------
C                                       zero output buffer
      IF (FPASS.LE.1) THEN
         I = NIFO * IFLEN
         CALL RFILL (I, 0.0, BUFOUT)
         END IF
C                                       copy each IF in order
      DO 100 I = 1,NIFO
         CALL GETORD (ORDER, I, INUM, J)
         IF (INUM.EQ.INFIL) THEN
            START = IFLEN * (J-1) + 1
            STOP = IFLEN * (I-1) + 1
            CALL RCOPY (IFLEN, BUFFIN(START), BUFOUT(STOP))
            END IF
  100    CONTINUE
C
 999  RETURN
      END
      SUBROUTINE LOADTB (FPASS, RTYPE, ORDER, NIFO, INFIL, DIMTWO,
     *   BUFFI, BUFFIO, BUFFR, BUFFRO, BUFFD, BUFFDO)
C-----------------------------------------------------------------------
C  Routine that loads up the output buffer using the IF ORDER
C  defined. Version for binary tables tables.
C   Inputs:
C     FPASS      i          If 1 is the first pass through the data
C                           so must ensure that all unused space in
C                           BUFFIO is blanked. Else it doesn't matter.
C     RTYPE      I          Data type, 1 = DP, 2 = SP, 3=CHAR, 4 = I
C     ORDER      I(*)       Order of IFs
C     NIFO       I          # IFs in output file
C     INFIL      I          File number of input data stream
C     BUFFI      R(*)       Buffer containing input data stream
C     DIMTWO     I          Size of 2nd dimension (in # of RTYPE words),
C                           always is the most rapidly varying.
C   Outputs:
C     BUFFIO     R(*)       Buffer containing output data stream
C-----------------------------------------------------------------------
      INTEGER   FPASS, RTYPE, ORDER(*), NIFO, INFIL, DIMTWO,
     *   BUFFI(*), BUFFIO(*)
      REAL    BUFFR(*), BUFFRO(*)
      DOUBLE PRECISION BUFFD(*), BUFFDO(*)
C
      INTEGER   I, J, INUM, START, STOP, LDIM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      LDIM = DIMTWO
      IF (RTYPE.EQ.3) LDIM = (DIMTWO + 3) / 4
      IF (FPASS.EQ.1) THEN
         I = NIFO * LDIM
         IF (RTYPE.EQ.1) CALL DFILL (I, DBLANK, BUFFDO)
         IF (RTYPE.EQ.2) CALL RFILL (I, 0.0, BUFFRO)
         IF (RTYPE.EQ.3) CALL RFILL (I, HBLANK, BUFFRO)
         IF (RTYPE.EQ.4) CALL FILL (I, 0, BUFFIO)
         END IF
      DO 100 I = 1, NIFO
         CALL GETORD (ORDER, I, INUM, J)
         IF (INUM.NE.INFIL) GO TO 100
         START = LDIM * (J-1) + 1
         STOP = LDIM * (I-1) + 1
         IF (RTYPE.EQ.1) CALL DPCOPY (LDIM, BUFFD(START), BUFFDO(STOP))
         IF ((RTYPE.EQ.2) .OR. (RTYPE.EQ.3))
     *      CALL RCOPY (LDIM, BUFFR(START), BUFFRO(STOP))
         IF (RTYPE.EQ.4) CALL COPY (LDIM, BUFFI(START), BUFFIO(STOP))
  100    CONTINUE
      RETURN
      END
      SUBROUTINE GETVAR (DATP, KOLS, RECI, RECR, RECD, TIME, TIMEI, SRC,
     *   ANT, ARR, FQID)
C-----------------------------------------------------------------------
C  Routine which extracts the variables needed for table comparison from
C  a given table record.
C   Inputs:
C     DATP        I(256)     Column pointers etc
C     KOLS        I(*)       Columns to read
C     RECI        I(*)       Integer record
C     RECR        R(*)       Real record
C     RECD        D(*)       Double precision record
C   Outputs:
C     TIME        D          Time of centre of record (days)
C     TIMEI       R          Interval covered by record (days)
C     SRC         I          Source number
C     ANT         I          Antenna number
C     ARR         I          Subarray number
C     FQID        I          Freqid
C-----------------------------------------------------------------------
      INTEGER   DATP(256), KOLS(*), RECI(*), SRC, ANT, ARR, FQID
      REAL      RECR(*), TIMEI
      DOUBLE PRECISION RECD(*), TIME
C
      INTEGER   IKOL, LENGTH, RTYPE, IPTR
C-----------------------------------------------------------------------
C                                       Get time
      IKOL = KOLS(1)
      IPTR = DATP(IKOL)
      LENGTH = DATP(128+IKOL) / 10
      RTYPE = DATP(128+IKOL) - LENGTH * 10
      IF (RTYPE.EQ.1) TIME = RECD(IPTR)
      IF (RTYPE.EQ.2) TIME = RECR(IPTR)
C                                       time interval
      IKOL = KOLS(6)
      IF (IKOL.GT.0) THEN
         IPTR = DATP(IKOL)
         LENGTH = DATP(128+IKOL) / 10
         RTYPE = DATP(128+IKOL) - LENGTH * 10
         IF (RTYPE.EQ.1) TIMEI = RECD(IPTR)
         IF (RTYPE.EQ.2) TIMEI = RECR(IPTR)
      ELSE
         TIMEI = -1.0
         END IF
C                                       source id
      IKOL = KOLS(2)
      IF (IKOL.GT.0) THEN
         IPTR = DATP(IKOL)
         SRC = RECI(IPTR)
      ELSE
         SRC = -1
         END IF
C                                       antenna #
      IKOL = KOLS(3)
      IF (IKOL.GT.0) THEN
         IPTR = DATP(IKOL)
         ANT = RECI(IPTR)
      ELSE
         ANT = -1
         END IF
C                                       array #
      IKOL = KOLS(4)
      IF (IKOL.GT.0) THEN
         IPTR = DATP(IKOL)
         ARR = RECI(IPTR)
      ELSE
         ARR = -1
         END IF
C                                       freqid #
      IKOL = KOLS(5)
      IF (IKOL.GT.0) THEN
         IPTR = DATP(IKOL)
         FQID = RECI(IPTR)
      ELSE
         FQID = -1
         END IF
C
 999  RETURN
      END
      SUBROUTINE CTTAB (IERR)
C-----------------------------------------------------------------------
C   Compares CT tables from all files to see if they are identicle
C   if not then do not copy any tables, if they are copy first table
C   and change the # of IFs in header.
C   Output:
C      IERR   I   Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'VBGLU.INC'
      INCLUDE 'CATS.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'TIMLST.INC'
      INCLUDE 'INCS:DCTV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INTEGER IN, BUFFER(512,4), LUNIN(4), ICTRNO(4), CTKOLS(12,4),
     *   CTNUMV(12,4), IN2, NREC(4), ICT, IER
      REAL TINY
      DOUBLE PRECISION TIME(4), UT1UTC(4), IATUTC(4), A1IAT(4),
     *   WOBXY(2,4), DPSI(4), DDPSI(4), DEPS(4), DDEPS(4), TRANGE(2,4)
      CHARACTER UT1TYP(4)*1, WOBTYP(4)*1
C-----------------------------------------------------------------------
      TINY = 1.0E-6
      CALL FILL (4, 0, LUNIN)
      DO 10 IN = 1,NUMFIL
         LUNIN(IN) = 40 + IN
C                                              initialize CT tables
         CALL CTINI ('READ', BUFFER(1,IN), DISKI(IN), CNOI(IN), 1,
     *      CATI(1,IN), LUNIN(IN), ICTRNO, CTKOLS(1,IN),
     *      CTNUMV(1,IN), IERR)
         IF (IERR.GT.0) THEN
            LUNIN(IN) = 0
            GO TO 70
            END IF
10       CONTINUE
      IERR = 1
C                                              Compare number of rows
C                                              in each table
      DO 30 IN = 1,NUMFIL-1
         NREC(IN) = BUFFER(5,IN)
         DO 20 IN2 = IN+1,NUMFIL
            NREC(IN2) = BUFFER(5,IN2)
            IF (NREC(IN).NE.NREC(IN2)) THEN
               WRITE (MSGTXT,1000) IN, IN2
               CALL MSGWRT (6)
               GO TO 70
               END IF
20          CONTINUE
30       CONTINUE
C                                              Compare individual values
C                                              in each column
      DO 60 ICT = 1,NREC(1)
         DO 50 IN = 1,NUMFIL-1
            ICTRNO(IN) = ICT
            CALL TABCT ('READ', BUFFER(1,IN), ICTRNO(IN), CTKOLS(1,IN),
     *         CTNUMV(1,IN),
     *         TIME(IN), UT1UTC(IN), IATUTC(IN), A1IAT(IN), UT1TYP(IN),
     *         WOBXY(1,IN), WOBTYP(IN), DPSI(IN), DDPSI(IN), DEPS(IN),
     *         DDEPS(IN), TRANGE(1,IN), IER)
            IF (IER.GT.0) GO TO 70
            DO 40 IN2 = IN+1, NUMFIL
               ICTRNO(IN2) = ICT
               CALL TABCT ('READ', BUFFER(1,IN2), ICTRNO(IN2),
     *            CTKOLS(1,IN2),
     *            CTNUMV(1,IN2), TIME(IN2), UT1UTC(IN2), IATUTC(IN2),
     *            A1IAT(IN2), UT1TYP(IN2), WOBXY(1,IN2), WOBTYP(IN2),
     *            DPSI(IN2), DDPSI(IN2), DEPS(IN2), DDEPS(IN2),
     *            TRANGE(1,IN2), IER)
               IF (IER.GT.0) GO TO 70
               IF (.NOT.((TIME(IN)+TINY.GT.TIME(IN2)).AND.
     *            (TIME(IN)-TINY.LT.TIME(IN2))))  THEN
                  WRITE (MSGTXT,1010) 'TIME   ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((UT1UTC(IN)+TINY.GT.UT1UTC(IN2)).AND.
     *            (UT1UTC(IN)-TINY.LT.UT1UTC(IN2))))  THEN
                  WRITE (MSGTXT,1010) 'UT1UTC ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((IATUTC(IN)+TINY.GT.IATUTC(IN2)).AND.
     *            (IATUTC(IN)-TINY.LT.IATUTC(IN2))))  THEN
                  WRITE (MSGTXT,1010) 'IATUTC ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((A1IAT(IN)+TINY.GT.A1IAT(IN2)).AND.
     *            (A1IAT(IN)-TINY.LT.A1IAT(IN2))))  THEN
                  WRITE (MSGTXT,1010) 'A1IAT  ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (UT1TYP(IN).NE.UT1TYP(IN2)) THEN
                  WRITE (MSGTXT,1010) 'UT1TYP ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((WOBXY(1,IN)+TINY.GT.WOBXY(1,IN2)).AND.
     *            (WOBXY(1,IN)-TINY.LT.WOBXY(1,IN2))))  THEN
                  WRITE (MSGTXT,1010) 'WOBXY1 ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((WOBXY(2,IN)+TINY.GT.WOBXY(2,IN2)).AND.
     *            (WOBXY(2,IN)-TINY.LT.WOBXY(2,IN2))))  THEN
                  WRITE (MSGTXT,1010) 'WOBXY2 ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (WOBTYP(IN).NE.WOBTYP(IN2)) THEN
                  WRITE (MSGTXT,1010) 'WOTYP  ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((DPSI(IN)+TINY.GT.DPSI(IN2)).AND.
     *            (DPSI(IN)-TINY.LT.DPSI(IN2))))  THEN
                  WRITE (MSGTXT,1010) 'DPSI   ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((DDPSI(IN)+TINY.GT.DDPSI(IN2)).AND.
     *            (DDPSI(IN)-TINY.LT.DDPSI(IN2))))  THEN
                  WRITE (MSGTXT,1010) 'DDPSI  ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((DEPS(IN)+TINY.GT.DEPS(IN2)).AND.
     *            (DEPS(IN)-TINY.LT.DEPS(IN2))))  THEN
                  WRITE (MSGTXT,1010) 'DEPS   ', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((TRANGE(1,IN)+TINY.GT.TRANGE(1,IN2)).AND.
     *            (TRANGE(1,IN)-TINY.LT.TRANGE(1,IN2))))  THEN
                  WRITE (MSGTXT,1010) 'TRANGE1', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
               IF (.NOT.((TRANGE(2,IN)+TINY.GT.TRANGE(2,IN2)).AND.
     *            (TRANGE(2,IN)-TINY.LT.TRANGE(2,IN2))))  THEN
                  WRITE (MSGTXT,1010) 'TRANGE2', ICT, IN, IN2
                  CALL MSGWRT (6)
                  GO TO 70
                  END IF
 40            CONTINUE
 50         CONTINUE
 60      CONTINUE
C                                              All is O.K., so set
C                                              IERR = 0
      IERR = 0
C                                              Close files
 70   DO 80 IN = 1,NUMFIL
         CALL TABCT('CLOS', BUFFER(1,IN), ICTRNO(IN), CTKOLS(1,IN),
     *      CTNUMV(1,IN), TIME(IN), UT1UTC(IN), IATUTC(IN), A1IAT(IN),
     *      UT1TYP(IN), WOBXY(1,IN), WOBTYP(IN), DPSI(IN), DDPSI(IN),
     *      DEPS(IN), DDEPS(IN), TRANGE(1,IN), IERR)
 80      CONTINUE
C                                              All is not O.K., so
C                                              print message and
C                                              set IERR
      IF (IERR.NE.0) THEN
         MSGTXT = '**********************************************' //
     *      '********'
         CALL MSGWRT (6)
         MSGTXT = '** CT tables are not identical, will not copy ' //
     *      'to    **'
         CALL MSGWRT (6)
         MSGTXT = '** output dataset.  See EXPLAIN file for more ' //
     *      'info. **'
         CALL MSGWRT (6)
         MSGTXT = '**********************************************' //
     *      '********'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('** Number of records not the same in CT tables ',
     *   I2,' and ',I2)
 1010 FORMAT ('CT table col. ',A7,' not the same in row ',I3,
     *   ' in files ',I1,' and ',I1)
      END
