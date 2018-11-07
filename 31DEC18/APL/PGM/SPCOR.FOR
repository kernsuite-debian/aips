LOCAL INCLUDE 'SPCOR.INC'
C                                       Local include for SPCOR
C                                       inputs, headers
      HOLLERITH XNAME1(3), XNAME2(3), XNAME3(3), XCLAS1(2), XCLAS2(2),
     *   XCLAS3(2), XNAMEO(3), XCLASO(2)
      REAL      XSEQ1, XSEQ2, XSEQ3, XDISK1, XDISK2, XDISK3, XBLC(7),
     *   XTRC(7), XSEQO, XDISKO, PBPARM(7), XFREQ, XCOORD(6), RADIUS,
     *   DOBLNK, DOINV
      CHARACTER NAME1*12, NAME2*12, NAME3*12, NAMEO*12, CLAS1*6,
     *    CLAS2*6, CLAS3*6, CLASO*6
      INTEGER   SEQ1, SEQ2, SEQ3, SEQO, DISK1, DISK2, DISK3, DISKO,
     *   CATII(256,3), CATOI(256), CNO1, CNO2, CNO3, CNOO
      REAL      CATIR(256,3), CATOR(256)
      HOLLERITH CATIH(256,3), CATOH(256)
      DOUBLE PRECISION CATID(128,3), CATOD(128)
      EQUIVALENCE (CATII, CATIH, CATIR, CATID)
      EQUIVALENCE (CATOI, CATOR, CATOH, CATOD)
      COMMON /INPARM/ XNAME1, XCLAS1, XSEQ1, XDISK1, XNAME2, XCLAS2,
     *   XSEQ2, XDISK2, XNAME3, XCLAS3, XSEQ3, XDISK3, XBLC, XTRC,
     *   XNAMEO, XCLASO, XSEQO, XDISKO, PBPARM, XFREQ, XCOORD, RADIUS,
     *   DOBLNK, DOINV
      COMMON /HDRS/ CATII, CATOI
      COMMON /OTHERS/ SEQ1, SEQ2, SEQ3, SEQO, DISK1, DISK2, DISK3,
     *   DISKO, CNO1, CNO2, CNO3, CNOO
      COMMON /OTHERC/ NAME1, CLAS1, NAME2, CLAS2, NAME3, CLAS3, NAMEO,
     *   CLASO
C                                       buffers
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SCRBUF(512), JBUFSZ
      REAL      BUFF1(MABFSL), BUFF2(MABFSL)
      COMMON /BUFRS/ BUFF1, BUFF2, SCRBUF, JBUFSZ

C                                       coordinate parameters
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LDEP(7), FQAX
      REAL      XPIX0, YPIX0, XINC, YINC
      DOUBLE PRECISION FREQS(MAXCHA), RAD0, DECD0, FRL0, POL0, COSR,
     *   SINR, RREFR, DREFR, RPTR, DPTR, DPTRC, DPTRS, X, Y, LAMBDA,
     *   RLAMBD, REFREQ
      COMMON /PBCHDR/ FREQS, X, Y, LAMBDA, RLAMBD, RAD0, DECD0, FRL0,
     *   POL0, COSR, SINR, RREFR, DREFR, RPTR, DPTR, DPTRC, DPTRS,
     *   REFREQ, XPIX0, YPIX0, XINC, YINC, LDEP, FQAX
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
LOCAL END
      PROGRAM SPCOR
C-----------------------------------------------------------------------
C! Task to correct an image for the primary beam and spectral index
C# Map-util Map
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012, 2015
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
C   SPCOR corrects an image for the primary beam response and/or for the
C   spectral index shown in a 2nd (and 3rd) image.  The primary image
C   may be a cube in any transposition, the spectral index images must
C   dingle planes.
C   INPUTS:
C      INNAME(3)                Image name (name)
C      INCLASS(2)                 Image name (class)
C      INSEQ                      Image name (seq. #)
C      INDISK                     Disk unit # of image
C      IN2NAME(3)               Image name (name) Spectral index
C      IN2CLASS(2)                Image name (class)
C      IN2SEQ                     Image name (seq. #)
C      IN2DISK                    Disk unit # of image
C      IN3NAME(3)               Image name (name) curvature
C      IN3CLASS(2)                Image name (class)
C      IN3SEQ                     Image name (seq. #)
C      IN3DISK                    Disk unit # of image
C      BLC(7)                   Bottom left corner to model
C      TRC(7)                   Top right corner to model
C      OUTNAME (3)              Output image name (name)
C      OUTCLASS(2)                Output image name (class)
C      OUTSEQ                     Output image name (seq. #)
C      OUTDISK                    Output image disk
C      PBPARM.......Control parameters:
C             (1)..Primary beam cutoff level.  0 => .023.  Pixels
C                  having lower sensitivity are blanked. >= 1 => no
C                  primary beam
C             (2)..> 0 => use PBPARM(3)-DPARM(7) to describe the
C                  primary beam.
C                  <= 0 => use parameters appropriate to VLA.
C             (3)..The beam is described by the function:
C                     PBPARM(3) + X*PBPARM(4) + X*X*PBPARM(5) +
C                        X*X*X*PBPARM(6) + X*X*X*X*PBPARM(7)
C                  where X is the square of the angular distance from
C                  the pointing position in arc minutes times the
C                  frequency in GHz.
C      X                        Reference to this frequency in GHz
C      RA(4)                    Pointing right ascension
C      DEC(4)                   Pointing declination
C      DOINVERS                 do inverse of correction
C-----------------------------------------------------------------------
      INTEGER   IRET, NWORDS, NIMAGE, NX2, NX3, NY2, NY3
      CHARACTER PRGNAM*6
      LONGINT   PSP0, PSP1
      REAL      SP0(2), SP1(2)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'SPCOR.INC'
      DATA PRGNAM /'SPCOR'/
C-----------------------------------------------------------------------
C                                       get inputs, create output
      CALL SPCORI (PRGNAM, NIMAGE, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       read in spectral index images
      PSP0 = 0
      PSP1 = 0
      IF (NIMAGE.GE.2) THEN
         NX2 = CATII(KINAX,2)
         NY2 = CATII(KINAX+1,2)
         NWORDS = (NX2 * NY2 - 1) / 1024 + 5
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SP0, PSP0, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY NEEDED'
            CALL MSGWRT (8)
            GO TO 900
            END IF
         CALL SPCORF (2, NX2, NY2, SP0(1+PSP0), IRET)
         IF (IRET.NE.0) GO TO 900
         END IF
      IF (NIMAGE.EQ.3) THEN
         NX3 = CATII(KINAX,2)
         NY3 = CATII(KINAX+1,3)
         NWORDS = (NX3 * NY3 - 1) / 1024 + 5
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SP1, PSP1, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY NEEDED'
            CALL MSGWRT (8)
            GO TO 900
            END IF
         CALL SPCORF (3, NX3, NY3, SP1(1+PSP1), IRET)
         IF (IRET.NE.0) GO TO 900
         END IF
C                                       do it
      CALL SPCORD (NIMAGE, NX2, NY2, SP0(1+PSP0), NX3, NY3, SP1(1+PSP1),
     *   IRET)
      IF (IRET.EQ.0) CALL SPCORH (NIMAGE)
C
 900  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE SPCORI (PRGNAM, NIMAGE, IRET)
C-----------------------------------------------------------------------
C   Gets the inputs for SPCOR and creates the output image
C   Inputs:
C      PRGNAM   C*6   Program name
C   Outputs:
C      NIMAGE   I     Number of input images 1, 2, or 3
C      IRET     I     Error code
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   NIMAGE, IRET
C
      INTEGER   NPARM, IERR, IROUND
      CHARACTER PTYPE*2, STAT*4
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'SPCOR.INC'
C-----------------------------------------------------------------------
C                                       init system
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
      JBUFSZ = MABFSL
C                                       get input adverbs
      NPARM = 59
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XNAME1, SCRBUF, IERR)
      IRET = IERR
      IF (IERR.NE.0) THEN
         RQUICK = .FALSE.
         WRITE (MSGTXT,1000) IERR, 'GETTING ADVERB VALUES'
         GO TO 990
         END IF
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
C                                       hollerith -> char etc
      CALL H2CHR (12, 1, XNAME1, NAME1)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (12, 1, XNAME3, NAME3)
      CALL H2CHR (12, 1, XNAMEO, NAMEO)
      CALL H2CHR (6, 1, XCLAS1, CLAS1)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (6, 1, XCLAS3, CLAS3)
      CALL H2CHR (6, 1, XCLASO, CLASO)
      SEQ1 = IROUND (XSEQ1)
      SEQ2 = IROUND (XSEQ2)
      SEQ3 = IROUND (XSEQ3)
      SEQO = IROUND (XSEQO)
      DISK1 = IROUND (XDISK1)
      DISK2 = IROUND (XDISK2)
      DISK3 = IROUND (XDISK3)
      DISKO = IROUND (XDISKO)
C                                       find input image
      CNO1 = 1
      PTYPE = 'MA'
      CALL CATDIR ('SRCH', DISK1, CNO1, NAME1, CLAS1, SEQ1, PTYPE,
     *   NLUSER, STAT, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, NAME1, CLAS1, SEQ1, DISK1
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK1, CNO1, CATBLK, 'READ', SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ IMAGE 1 HEADER'
         GO TO 990
         END IF
      CALL COPY (256, CATBLK, CATII(1,1))
      NIMAGE = 1
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK1
      FCNO(NCFILE) = CNO1
      FRW(NCFILE) = 0
      IF ((NAME2.NE.' ') .AND. (CLAS2.NE.' ')) THEN
         CNO2 = 1
         PTYPE = 'MA'
         CALL CATDIR ('SRCH', DISK2, CNO2, NAME2, CLAS2, SEQ2, PTYPE,
     *      NLUSER, STAT, SCRBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, NAME2, CLAS2, SEQ2, DISK2
            GO TO 990
            END IF
         CALL CATIO ('READ', DISK2, CNO2, CATII(1,2), 'READ', SCRBUF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ IMAGE 2 HEADER'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK2
         FCNO(NCFILE) = CNO2
         FRW(NCFILE) = 0
         NIMAGE = 2
         END IF
      IF ((NIMAGE.EQ.2) .AND. (NAME3.NE.' ') .AND. (CLAS3.NE.' ')) THEN
         CNO3 = 1
         PTYPE = 'MA'
         CALL CATDIR ('SRCH', DISK3, CNO3, NAME3, CLAS3, SEQ3, PTYPE,
     *      NLUSER, STAT, SCRBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, NAME3, CLAS3, SEQ3, DISK3
            GO TO 990
            END IF
         CALL CATIO ('READ', DISK3, CNO3, CATII(1,3), 'READ', SCRBUF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ IMAGE 3 HEADER'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK3
         FCNO(NCFILE) = CNO3
         FRW(NCFILE) = 0
         NIMAGE = 3
         END IF
C                                       check windows
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), XBLC, XTRC, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SETTING WINDOW PARAMETERS'
         GO TO 990
         END IF
C                                       get frequency list
      CALL SPCORS (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       prepare output
      CALL MAKOUT (NAME1, CLAS1, SEQ1, PRGNAM, NAMEO, CLASO, SEQO)
      CALL SUBHDR (XBLC, XTRC, 1.0, 1.0)
      CALL CHR2H (12, NAMEO, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASO, KHIMCO, CATH(KHIMN))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQO
C                                       create it
      CALL MCREAT (DISKO, CNOO, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING OUTPUT IMAGE'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = 2
C                                       Copy any header keywords
      CALL KEYCOP (DISK1, CNO1, DISKO, CNOO, IRET)
      IRET = 0
      CALL COPY (256, CATBLK, CATOI)
C                                       pointing position
      RAD0 = ABS (XCOORD(1)) + ABS (XCOORD(2))/60.0D0 +
     *   ABS (XCOORD(3))/3600.0D0
      IF ((XCOORD(1).LT.0.0) .OR. (XCOORD(2).LT.0.0) .OR.
     *   (XCOORD(3).LT.0.0)) RAD0 = -RAD0
      RAD0 = RAD0 * 15.0D0
      DECD0 = ABS(XCOORD(4)) + ABS(XCOORD(5))/60.0D0 +
     *   ABS(XCOORD(6))/3600.0D0
      IF ((XCOORD(4).LT.0.0) .OR. (XCOORD(5).LT.0.0) .OR.
     *   (XCOORD(6).LT.0.0)) DECD0 = -DECD0
      IF ((RAD0.EQ.0.0D0) .AND. (DECD0.EQ.0.0D0)) THEN
         RAD0 = CATID(KDORA,1)
         DECD0 = CATID(KDODE,1)
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPCORI: ERROR',I3,' ON ',A)
 1010 FORMAT ('SPCORI: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3)
      END
      SUBROUTINE SPCORS (IRET)
C-----------------------------------------------------------------------
C   SPCORS computes or reads the list of frequencies
C   Outputs:
C      IRET   I   >0 => error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IAX, I, J, VER, LUN, IFQRNO, FQKOLS(MAXFQC), NF,
     *   FQNUMV(MAXFQC), NUMIF, FQID, IFSIDE, IREC, NREC, FAX, IBLC,
     *   ITRC
      DOUBLE PRECISION IFFREQ, CV, CI, CP, FV
      REAL      IFCHW, IFTBW
      CHARACTER TEMP*4, BNDCOD*8
      INCLUDE 'SPCOR.INC'
C-----------------------------------------------------------------------
      I = KHCTP
      IAX = 0
      FAX = 0
      DO 10 J = 1,CATII(KIDIM,1)
         CALL H2CHR (4, 1, CATIH(I,1), TEMP)
         I = I + 2
         IF (TEMP.EQ.'FREQ') FAX = J
         IF (TEMP.EQ.'FQID') IAX = J
 10      CONTINUE
      IF (FAX.LE.0) THEN
         MSGTXT = 'FREQ AXIS NOT FOUND'
         IRET = 10
         GO TO 990
         END IF
C                                       reference freq
      IF (XFREQ.GT.0.0) THEN
         REFREQ = XFREQ
      ELSE
         REFREQ = CATID(KDCRV+FAX-1,1) / 1.D9
         END IF
C                                       axis set up
      IF (IAX.GT.0) THEN
         J = IAX
      ELSE
         J = FAX
         END IF
      FQAX = J
      IBLC = XBLC(J) + 0.1
      ITRC = XTRC(J) + 0.1
      CV = CATID(KDCRV+J-1,1)
      CI = CATIR(KRCIC+J-1,1)
      CP = CATIR(KRCRP+J-1,1)
      FV = CATID(KDCRV+FAX-1,1)
      NF = CATII(KINAX+J-1,1)
C                                       regular axis
      IF (IAX.EQ.0) THEN
         DO 40 I = 1,NF
            FREQS(I) = CV + (I - CP) * CI
 40         CONTINUE
C                                       read FQ table
      ELSE
         VER = 1
         LUN = 44
         NUMIF = 1
         CALL FQINI ('READ', SCRBUF, DISK1, CNO1, VER, CATII, LUN,
     *      IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING FQ TABLE'
            GO TO 990
            END IF
         NREC = SCRBUF(5)
C                                       read
         CALL DFILL (ITRC, -1.0D0, FREQS)
         DO 50 IREC = 1,NREC
            CALL TABFQ ('READ', SCRBUF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE'
               GO TO 990
               END IF
            I = (FQID - CV) / CI + CP + 0.5
            IF ((I.GE.1) .AND. (I.LE.NF)) FREQS(I) = IFFREQ
     *         + FV
 50         CONTINUE
         CALL TABFQ ('CLOS', SCRBUF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IREC)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPCORS: ERROR',i3,' ON ',A)
      END
      SUBROUTINE SPCORF (N, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   SPCORF reads in image N to memory - note that it reads one plane
C   which should be all there is
C   Inputs:
C      N       I          image number (2, 3)
C      NX      I          number X pixels
C      NY      I          number Y pixels
C   Outputs:
C      IMAGE   R(NX,NY)   image values
C      IRET    I          error code
C-----------------------------------------------------------------------
      INTEGER   N, NX, NY, IRET
      REAL      IMAGE(NX,NY)
C
      INTEGER   WIN(4), DEPTH(5), BLKOF, IY, LUN, IND, IPOS
      CHARACTER PHNAME*48
      INCLUDE 'SPCOR.INC'
      DATA DEPTH /5*1/
      DATA LUN /44/
C-----------------------------------------------------------------------
C                                       init IO
      IF (N.EQ.2) THEN
         CALL ZPHFIL ('MA', DISK2, CNO2, 1, PHNAME, IRET)
         CALL ZOPEN (LUN, IND, DISK2, PHNAME, .TRUE., .TRUE., .TRUE.,
     *      IRET)
      ELSE IF (N.EQ.3) THEN
         CALL ZPHFIL ('MA', DISK3, CNO3, 1, PHNAME, IRET)
         CALL ZOPEN (LUN, IND, DISK3, PHNAME, .TRUE., .TRUE., .TRUE.,
     *      IRET)
      ELSE
         IRET = 10
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING SPECTRAL INDEX IMAGE'
         GO TO 990
         END IF
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
      CALL COMOFF (CATII(KIDIM,N), CATII(KINAX,N), DEPTH, BLKOF, IRET)
      BLKOF = BLKOF + 1
      CALL MINIT ('READ', LUN, IND, NX, NY, WIN, BUFF1, JBUFSZ, BLKOF,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO SPECTRAL INDEX IMAGE'
         GO TO 990
         END IF
C                                       read and close
      DO 20 IY = 1,NY
         CALL MDISK ('READ', LUN, IND, BUFF1, IPOS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING SPECTRAL INDEX IMAGE'
            GO TO 990
            END IF
         CALL RCOPY (NX, BUFF1(IPOS), IMAGE(1,IY))
 20      CONTINUE
      CALL ZCLOSE (LUN, IND, IRET)
C                                       set location common
      LOCNUM = N
      CALL COPY (256, CATII(1,N), CATBLK)
      CALL SETLOC (DEPTH, .FALSE.)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPCORF: ERROR',I3,' ON ',A)
      END
      SUBROUTINE SPCORH (NIMAGE)
C-----------------------------------------------------------------------
c   write HI to output
C-----------------------------------------------------------------------
      INTEGER   NIMAGE
C
      INTEGER   LUN1, LUN2, IERR, IBLC(7), ITRC(7), I, RA(2), DEC(2),
     *   NONOT
      REAL      RAS, DECS
      CHARACTER HILINE*72, CHRASN*1, CHDCSN*1, NOTTYP*2
      INCLUDE 'SPCOR.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTTYP /1, 'CC'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
      CALL COPY (256, CATOI, CATBLK)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISK1, DISKO, CNO1, CNOO, CATBLK,
     *   SCRBUF(257), SCRBUF, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 60
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAME1, CLAS1, SEQ1, DISK1, LUN2, SCRBUF,
     *   IERR)
      IF (IERR.NE.0) GO TO 60
      IF (NIMAGE.GE.2) THEN
         CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, SCRBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
      IF (NIMAGE.GE.3) THEN
         CALL HENCO3 (TSKNAM, NAME3, CLAS3, SEQ3, DISK3, LUN2, SCRBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
      CALL HENCOO (TSKNAM, NAMEO, CLASO, SEQO, DISKO, LUN2, SCRBUF,
     *   IERR)
      IF (IERR.NE.0) GO TO 60
C                                       corners
      DO 10 I = 1,7
         IBLC(I) = XBLC(I) + 0.1
         ITRC(I) = XTRC(I) + 0.1
 10      CONTINUE
      WRITE (HILINE,1010) TSKNAM, 'BLC', IBLC
      CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 60
      WRITE (HILINE,1010) TSKNAM, 'TRC', ITRC
      CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       if primary beam
      IF (PBPARM(1).LT.1.0) THEN
C                                          Pointing RA and DEC
         CALL COORDD (1, RAD0, CHRASN, RA, RAS)
         CALL COORDD (2, DECD0, CHDCSN, DEC, DECS)
         WRITE (HILINE,1020) TSKNAM, CHRASN, RA, RAS
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1021) TSKNAM, CHDCSN, DEC, DECS
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
C                                       cutoff level
         WRITE (HILINE,1025) TSKNAM, PBPARM(1)
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
C                                       beam shape
         WRITE (HILINE,1030) TSKNAM, 1.0
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1031) TSKNAM, PBPARM(3)
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1032) TSKNAM, PBPARM(4)
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1033) TSKNAM, PBPARM(5)
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1034) TSKNAM, PBPARM(6)
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1035) TSKNAM, PBPARM(7)
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         WRITE (HILINE,1036) TSKNAM
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
C                                       no primary beam
      ELSE
         HILINE = TSKNAM // '/ No primary beam correction applied'
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
C                                       Reference freq
      WRITE (HILINE,1040) TSKNAM, REFREQ
      CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       spectral index parms
      IF (NIMAGE.GE.2) THEN
         WRITE (HILINE,1041) TSKNAM, RADIUS
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         IF (DOBLNK.GT.0.0) THEN
            HILINE = TSKNAM // '/ output blanked when spectral index'
     *         // ' blanked'
         ELSE
            HILINE = TSKNAM // '/ output not blanked when spectral'
     *         // ' index blanked'
            END IF
         END IF
C                                       DOINVERS
      IF (DOINV.GT.0.0) THEN
         HILINE = TSKNAM // '/ inverse correction actually done'
         CALL HIADD (LUN2, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 60
         END IF
      GO TO 70
C                                       HI error
 60   WRITE (MSGTXT,1060) IERR
      CALL MSGWRT (7)
C
 70   CALL HICLOS (LUN2, .TRUE., SCRBUF, IERR)
C                                       Copy tables
C                                       omit CC files - not true after
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISK1, DISKO, CNO1, CNOO,
     *   CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GE.1) THEN
         WRITE (MSGTXT,1070) IERR
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPCORH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,A,'=',3(I6,','),3(I4,','),I4)
 1020 FORMAT (A6,'PRA= ',A1,I2.2,I3.2,F7.3,'  / pointing position')
 1021 FORMAT (A6,'PDec= ',A1,I2.2,I3.2,F6.2,'  / pointing position')
 1025 FORMAT (A6,'Beamcut=',F9.5,3X,'/Clip outside this beam value')
 1030 FORMAT (A6,'/ Beam = ',1PE13.6,' +',5X,'/ Beam  model')
 1031 FORMAT (A6,'/',8X,1PE13.6,' * X +')
 1032 FORMAT (A6,'/',8X,1PE13.6,' * X*X +')
 1033 FORMAT (A6,'/',8X,1PE13.6,' * X*X*X +')
 1034 FORMAT (A6,'/',8X,1PE13.6,' * X*X*X*X')
 1035 FORMAT (A6,'/',8X,1PE13.6,' * X*X*X*X*X')
 1036 FORMAT (A6,'/ Where X = [Angle(arcmin) * f(GHz)]**2')
 1040 FORMAT (A6,'RefFreq=',F9.4,' / GHz reference frequency')
 1041 FORMAT (A6,'RADIUS=',F7.2,' / pixels smoothing radius')
 1060 FORMAT ('CANNOT ADD LINES TO HI FILE.  IER=',I8)
 1070 FORMAT ('ERROR ',I3,' COPYING TABLES')
      END
      SUBROUTINE SPCORD (NIMAGE, NX2, NY2, SP0, NX3, NY3, SP1, IRET)
C-----------------------------------------------------------------------
C   SPCORD actually reads the input and causes the corrections to be
C   made to the output
C   Inputs:
C      NIMAGE   I            Number images used
C      NX2      I            Number X pixels spectral index image
C      NY2      I            Number Y pixels spectral index image
C      SP0      R(NX2,NY2)   Spectral index image
C      NX3      I            Number X pixels spectral curvature image
C      NY3      I            Number Y pixels spectral curvature image
C      SP1      R(NX3,NY3)   Spectral curvature image
C   Outputs:
C      IRET     I            Error code
C-----------------------------------------------------------------------
      INTEGER   NIMAGE, NX2, NY2, NX3, NY3, IRET
      REAL      SP0(NX2,NY2), SP1(NX3,NY3)
C
      INTEGER   I3, I4, I5, I6, I7, IBLC(7), ITRC(7), IDEPTH(7), LUNI,
     *   LUNO, NX, NY, IWIN(4), NXO, NYO, OWIN(4), INDI, INDO, IPOSI,
     *   IPOSO, ODEPTH(5), I, IBLKOF, IX, IY, IP
      REAL      DMAX, DMIN, CORFAC
      LOGICAL   BLANKD
      CHARACTER PHNAME*48
      INCLUDE 'SPCOR.INC'
      DATA LUNI, LUNO /27,28/
C-----------------------------------------------------------------------
      BLANKD = .FALSE.
      DMAX = -1.E20
      DMIN = 1.E20
C                                       windows
      DO 10 I = 1,7
         IBLC(I) = XBLC(I) + 0.1
         ITRC(I) = XTRC(I) + 0.1
 10      CONTINUE
      NX = CATII(KINAX,1)
      NY = CATII(KINAX+1,1)
      IWIN(1) = IBLC(1)
      IWIN(2) = IBLC(2)
      IWIN(3) = ITRC(1)
      IWIN(4) = ITRC(2)
      NXO = CATOI(KINAX)
      NYO = CATOI(KINAX+1)
      OWIN(1) = 1
      OWIN(2) = 1
      OWIN(3) = NXO
      OWIN(4) = NYO
C                                       open files
      CALL ZPHFIL ('MA', DISK1, CNO1, 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISK1, PHNAME, .TRUE., .TRUE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE'
         GO TO 990
         END IF
      CALL ZPHFIL ('MA', DISKO, CNOO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, .TRUE., .TRUE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT IMAGE'
         GO TO 990
         END IF
C                                       I/O
      IDEPTH(1) = IBLC(1)
      IP = 0
      CALL COPY (256, CATII(1,1), CATBLK)
      DO 100 I7 = IBLC(7),ITRC(7)
      DO 100 I6 = IBLC(6),ITRC(6)
      DO 100 I5 = IBLC(5),ITRC(5)
      DO 100 I4 = IBLC(4),ITRC(4)
      DO 100 I3 = IBLC(3),ITRC(3)
         IP = IP + 1
         IF (MOD(IP,20).EQ.0) THEN
            WRITE (MSGTXT,1001) IP
            CALL MSGWRT (2)
            END IF
C                                       position computing init
         CALL FILL (7, 0, LDEP)
C                                       Set corner selection.
         IDEPTH(3) = I3
         IDEPTH(4) = I4
         IDEPTH(5) = I5
         IDEPTH(6) = I6
         IDEPTH(7) = I7
C                                       Block offset for source file.
         CALL COMOFF (CATII(KIDIM,1), CATII(KINAX,1), IDEPTH(3), IBLKOF,
     *      IRET)
         IBLKOF = IBLKOF + 1
C                                       coordinate setup for file 1
         LOCNUM = 1
         CALL SETLOC (IDEPTH(3), .FALSE.)
C                                       init read
         CALL MINIT ('READ', LUNI, INDI, NX, NY, IWIN, BUFF1, JBUFSZ,
     *      IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT I/O FROM INPUT IMAGE'
            GO TO 990
            END IF
C                                       output
         ODEPTH(1) = I3 - IBLC(3) + 1
         ODEPTH(2) = I4 - IBLC(4) + 1
         ODEPTH(3) = I5 - IBLC(5) + 1
         ODEPTH(4) = I6 - IBLC(6) + 1
         ODEPTH(5) = I7 - IBLC(7) + 1
C                                       Block offset for source file.
         CALL COMOFF (CATOI(KIDIM), CATOI(KINAX), ODEPTH, IBLKOF, IRET)
         IBLKOF = IBLKOF + 1
C                                       init read
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, OWIN, BUFF2, JBUFSZ,
     *      IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT I/O TO OUTPUT IMAGE'
            GO TO 990
            END IF
C                                       read/write plane
         DO 70 IY = IBLC(2),ITRC(2)
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IPOSI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
               GO TO 990
               END IF
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, IPOSO, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT IMAGE'
               GO TO 990
               END IF
            IDEPTH(2) = IY
            DO 50 IX = 1,NXO
               IF (BUFF1(IPOSI+IX-1).NE.FBLANK) THEN
                  IDEPTH(1) = IX + IBLC(1) - 1
                  CALL SPDOIT (IDEPTH, NIMAGE, NX2, NY2, SP0, NX3, NY3,
     *               SP1, CORFAC, IRET)
               ELSE
                  CORFAC = 0.0
                  IRET = 0
                  END IF
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET,
     *               'RETURNED FROM SPEC INDEX CORR'
                  GO TO 990
               ELSE IF (CORFAC.LE.0.0) THEN
                  BUFF2(IPOSO+IX-1) = FBLANK
                  BLANKD = .TRUE.
               ELSE
                  IF (DOINV.GT.0.0) THEN
                     BUFF2(IPOSO+IX-1) = BUFF1(IPOSI+IX-1) * CORFAC
                  ELSE
                     BUFF2(IPOSO+IX-1) = BUFF1(IPOSI+IX-1) / CORFAC
                     END IF
                  DMAX = MAX (DMAX, BUFF2(IPOSO+IX-1))
                  DMIN = MIN (DMAX, BUFF2(IPOSO+IX-1))
                  END IF
 50            CONTINUE
 70         CONTINUE
         CALL MDISK ('FINI', LUNO, INDO, BUFF2, IPOSO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINISHING OUTPUT IMAGE'
            GO TO 990
            END IF
 100     CONTINUE
C                                       close
      CALL ZCLOSE (LUNI, INDI, IY)
      CALL ZCLOSE (LUNO, INDO, IY)
C                                       extrema
      CATOR(KRDMX) = DMAX
      CATOR(KRDMN) = DMIN
      IF (BLANKD) THEN
         CATOR(KRBLK) = FBLANK
      ELSE
         CATOR(KRBLK) = 0.0
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPCORD: ERROR',I3,' ON ',A)
 1001 FORMAT ('SPCORD: at plane',I6)
      END
      SUBROUTINE SPDOIT (IPOS, NIMAGE, NX2, NY2, SP0, NX3, NY3, SP1,
     *   CORFAC, IRET)
C-----------------------------------------------------------------------
C   SPDOIT does all the hard work - computes the coordinate at each
C   pixel, gets the spectral index image value, gets the primary beam
C   value and corrects the output.
C   Inputs:
C      IPOS     I(7)         Input image pixel position
C      NIMAGE   I            Number images used
C      NX2      I            Number X pixels spectral index image
C      NY2      I            Number Y pixels spectral index image
C      SP0      R(NX2,NY2)   Spectral index image
C      NX3      I            Number X pixels spectral curvature image
C      NY3      I            Number Y pixels spectral curvature image
C      SP1      R(NX3,NY3)   Spectral curvature image
C   Outputs:
C      CORFAC   R            Correction factor to divide into image
C      IRET     I            error code
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), NIMAGE, NX2, NY2, NX3, NY3, IRET
      REAL      SP0(NX2,NY2), SP1(NX3,NY3), CORFAC
C
      REAL      PBCORR, PBCORF, SPCORF, SPCORR
      DOUBLE PRECISION DX, DY, DT
      LOGICAL   FIRST, DOREF, OUTSID
      CHARACTER ARRAY*8
      INCLUDE 'SPCOR.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE FIRST, PBCORR, SPCORR, ARRAY, DOREF, DX, DY, DT
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
      DOREF = .FALSE.
C                                       Pointing pixel 1st time in
      IF (FIRST) THEN
         IF ((RAD0.EQ.0.0D0) .AND. (DECD0.EQ.0.0D0)) THEN
            RAD0 = CATID(KDCRV+KLOCL(LOCNUM)-1,1)
            DECD0 = CATID(KDCRV+KLOCM(LOCNUM)-1,1)
            END IF
         CALL H2CHR (8, 1, CATIH(KHTEL,1), ARRAY)
         RLAMBD = VELITE / (REFREQ * 1.D9)
         DOREF = .TRUE.
         XINC = CATIR(KRCIC+KLOCL(LOCNUM),1) * COND2R
         YINC = CATIR(KRCIC+KLOCM(LOCNUM),1) * COND2R
         IRET = 2
         IF ((XINC.EQ.0.0) .OR. (YINC.EQ.0.0)) GO TO 999
         COSR = COS (ROT(LOCNUM) * COND2R)
         SINR = SGNROT(LOCNUM) * SIN (ROT(LOCNUM) * COND2R)
         RREFR = CATID(KDCRV+KLOCL(LOCNUM),1) * COND2R
         DREFR = CATID(KDCRV+KLOCM(LOCNUM),1) * COND2R
         RPTR = RAD0 * COND2R
         DPTR = DECD0 * COND2R
         DPTRS = SIN (DPTR)
         DPTRC = COS (DPTR)
         CALL DIRCOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RREFR, DREFR,
     *      RPTR, DPTR, X, Y, IRET)
         IF (IRET.NE.0) GO TO 999
         YPIX0 = (Y * COSR - X * SINR) / YINC +
     *      CATIR(KRCRP+KLOCM(LOCNUM),1)
         XPIX0 = (X * COSR + Y * SINR) / XINC +
     *      CATIR(KRCRP+KLOCL(LOCNUM),1)
         FIRST = .FALSE.
         END IF
C                                       Frequency
      IF (IPOS(KLOCF(LOCNUM)+1).NE.LDEP(KLOCF(LOCNUM)+1)) THEN
         FRL0 = FREQS(IPOS(KLOCF(LOCNUM)+1))
         LAMBDA = VELITE / FRL0
         END IF
C                                       Position term
      IF ((IPOS(KLOCL(LOCNUM)+1).NE.LDEP(KLOCL(LOCNUM)+1)) .OR.
     *   (IPOS(KLOCM(LOCNUM)+1).NE.LDEP(KLOCM(LOCNUM)+1))) THEN
         DOREF = .TRUE.
C                                       Accurate method
C                                       Offset, rotation to position
         DX = (IPOS(KLOCL(LOCNUM)+1) - CATIR(KRCRP+KLOCL(LOCNUM),1))
     *      * XINC
         DY = (IPOS(KLOCM(LOCNUM)+1) - CATIR(KRCRP+KLOCM(LOCNUM),1))
     *      * YINC
         DT = DX * COSR - DY * SINR
         DY = DY * COSR + DX * SINR
         DX = DT
         CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RREFR, DREFR,
     *      DX, DY, X, Y, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       get actual angle
         DT = DPTRS * SIN(Y) + DPTRC * COS(Y) * COS(RPTR-X)
         IF (DT.GT.1.0D0) DT = 1.0D0
         IF (DT.LT.-1.0D0) DT = -1.0D0
         POL0 = RAD2DG * ACOS (DT)
         END IF
C                                       PB correction factor
      IF (PBPARM(1).GE.1.0) THEN
         PBCORF = 1.0
         PBCORR = 1.0
      ELSE
         CALL PBCALC (POL0, LAMBDA, ARRAY, PBPARM(2), PBCORF, OUTSID)
         IF ((OUTSID) .OR. (PBCORF.LT.PBPARM(1))) PBCORF = 0.0
         IF (DOREF) THEN
            CALL PBCALC (POL0, RLAMBD, ARRAY, PBPARM(2), PBCORR, OUTSID)
            IF ((OUTSID) .OR. (PBCORR.LT.PBPARM(1))) PBCORR = 0.0
            END IF
         IF (PBCORR.GT.0.0) THEN
            PBCORF = PBCORF / PBCORR
         ELSE
            PBCORF = 0.0
            END IF
         END IF
C                                       SP index correction
      IF (NIMAGE.LT.2) THEN
         SPCORR = 1.0
         SPCORF = 1.0
      ELSE
         CALL SPCALC (X, Y, LAMBDA, NIMAGE, NX2, NY2, SP0, NX3, NY3,
     *      SP1, SPCORF)
         IF (DOREF) CALL SPCALC (X, Y, RLAMBD, NIMAGE, NX2, NY2, SP0,
     *      NX3, NY3, SP1, SPCORR)
         IF (SPCORR.GT.0.0) THEN
            SPCORF = SPCORF / SPCORR
         ELSE IF (DOBLNK.GT.0.0) THEN
            SPCORF = 0.0
         ELSE
            SPCORF = 1.0
            END IF
         END IF
C                                       final corr
      CORFAC = PBCORF * SPCORF
C                                       remember where we were
      CALL COPY (7, IPOS, LDEP)
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE SPCALC (XX, YY, LAMB, NIMAGE, NX2, NY2, SP0, NX3, NY3,
     *   SP1, CORFAC)
C-----------------------------------------------------------------------
C   SPCALC finds the spectral index for the specified coordinate and
C   returns a correction factor to divide into the image to get the
C   image at 1 GHz.
C   Inputs
C      XX       D            RA in radians
C      YY       D            DEC in radians
C      LAMBD    D            Wavelength in meters
C      NIMAGE   I            Number images used
C      NX2      I            Number X pixels spectral index image
C      NY2      I            Number Y pixels spectral index image
C      SP0      R(NX2,NY2)   Spectral index image
C      NX3      I            Number X pixels spectral curvature image
C      NY3      I            Number Y pixels spectral curvature image
C      SP1      R(NX3,NY3)   Spectral curvature image
C   Output:
C      CORFAC   R            Correction factor: 0.0 -> problem
C-----------------------------------------------------------------------
      INTEGER   NIMAGE, NX2, NY2, NX3, NY3
      REAL      SP0(NX2,NY2), SP1(NX3,NY3), CORFAC
      DOUBLE PRECISION XX, YY, LAMB
C
      INTEGER   IRET
      REAL      XPIX, YPIX, SP0VAL, SP1VAL
      DOUBLE PRECISION FREQ, DX, DY
      INCLUDE 'SPCOR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CORFAC = 0.0
      IF (RADIUS.LT.SQRT(0.5)) RADIUS = SQRT (0.5)
C                                       frequency
      FREQ = VELITE / LAMB / 1.D9
      FREQ = LOG10 (FREQ)
      DX = XX * RAD2DG
      DY = YY * RAD2DG
C                                       spectral index image
      LOCNUM = 2
      CALL XYPIX (DX, DY, XPIX, YPIX, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SPTERP (NX2, NY2, SP0, XPIX, YPIX, RADIUS, SP0VAL)
      IF (SP0VAL.EQ.FBLANK) GO TO 999
C                                       curvature
      IF (NIMAGE.LE.2) THEN
         SP1VAL = 0.0
      ELSE
         LOCNUM = 3
         CALL XYPIX (DX, DY, XPIX, YPIX, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL SPTERP (NX3, NY3, SP1, XPIX, YPIX, RADIUS, SP1VAL)
         IF (SP1VAL.EQ.FBLANK) GO TO 999
         END IF
C                                       restore main image
      LOCNUM = 1
C                                       corr factor
      CORFAC = (SP0VAL + SP1VAL * FREQ) * FREQ
      CORFAC = 10.0 ** CORFAC
C
 999  RETURN
      END
      SUBROUTINE SPTERP (NX, NY, SP, XPIX, YPIX, RADIUS, SPVAL)
C-----------------------------------------------------------------------
C   SPTERP finds the average value in image SP in a radius around a
C   pixel position
C   Inputs
C      NX       I          x dimension of SP
C      NY       I          Y dimension of SP
C      SP       R(NX,NY)   Image
C      XPIX     R          X pixel center
C      YPIX     R          Y pixel center
C      RADIUS   R          radius of averaging area
C   Outputs:
C      SPVAL    R          average value of FBLANK if fail or all blank
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      REAL      SP(NX,NY), XPIX, YPIX, RADIUS, SPVAL
C
      INTEGER   IX, X1, X2, IY, Y1, Y2
      REAL      SUM, SW, R, RR, W, RS
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SPVAL = FBLANK
C                                       set area
      RR = 2.0 * RADIUS
      X1 = XPIX - RR + 0.99
      X2 = XPIX + RR
      Y1 = YPIX - RR + 0.99
      Y2 = YPIX + RR
      X1 = MAX (1, X1)
      Y1 = MAX (1, Y1)
      X2 = MIN (NX, X2)
      Y2 = MIN (NY, Y2)
C                                       sum
      RS = RADIUS * RADIUS
      RR = 4. * RS
      SW = 0.0
      SUM = 0.0
      DO 20 IY = Y1,Y2
         DO 10 IX = X1,X2
            R = (IX-XPIX)**2 + (IY-YPIX)**2
            IF ((R.LE.RR) .AND. (SP(IX,IY).NE.FBLANK)) THEN
               W = EXP (-R / RS)
               SW = SW + W
               SUM = SUM + W * SP(IX,IY)
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       average
      IF (SW.GT.0) SPVAL = SUM / SW
C
 999  RETURN
      END
