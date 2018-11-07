LOCAL INCLUDE 'UVSUB.INC'
C                                                          Include UVSUB
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
C                                       NOTE: uses PARAMETER in DGDS.INC
      INTEGER   NCOMP(MAXFLD), BCOMP(MAXFLD)
      INTEGER   UVBLK(256), DISKIN, DISKO, DISK2, OLDCNO, CLNCNO,
     *   NEWCNO, JBUFSZ, SEQIN, SEQ2, SEQOUT, VER, CHAN, NCHAN, ISTOKE,
     *   METHOD, MODEL, BIF, EIF, CH1, FRQSEL
      LOGICAL   DIVIDE
      REAL   XSIN, XDISIN, XS2, XDISK2, XSOUT, XDISO, XNCOMP(MAXAFL),
     *   FACTOR, XBCOMP(MAXAFL), RAOFF, DECOFF, SMODEL(7), XVER, XNCH,
     *   XNMAPS, BADD(10), XBIF, XEIF, BUFF1(UVBFSS), BUFF2(UVBFSS),
     *   BUFF3(UVBFSS), XFLUX
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAS2(2), XNAMOU(3),
     *   XCLAOU(2), XOPCOD, XCMETH, XCMOD
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, NAMOUT*12,
     *   CLAOUT*6, OPCODE*4, CMETH*4, CMOD*4
      COMMON /INFO/ UVBLK, RAOFF, DECOFF,
     *   BCOMP, NCOMP, DIVIDE,
     *   DISKIN, DISKO, DISK2, OLDCNO, CLNCNO, NEWCNO, ISTOKE,
     *   JBUFSZ, SEQIN, SEQOUT, SEQ2, VER, CHAN, NCHAN, METHOD, MODEL,
     *   BIF, EIF, CH1, FRQSEL
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNMAPS, XNCH,
     *   XBIF, XEIF, XNAME2, XCLAS2, XS2, XDISK2, XVER,
     *   XNAMOU, XCLAOU, XSOUT, XDISO, XBCOMP, XNCOMP, XFLUX, XCMETH,
     *   XCMOD, FACTOR, XOPCOD, SMODEL, BADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAME2, CLAS2, NAMOUT, CLAOUT,
     *   OPCODE, CMETH, CMOD
C                                                          End UVSUB
LOCAL END
      PROGRAM UVSUB
C-----------------------------------------------------------------------
C! Subtracts (or divides) a model or image from (into) uv data.
C# UV AP-appl Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1999, 2003, 2007-2009, 2014, 2018
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
C   UVSUB is an AIPS system task to subtract a source model from a
C   uv-data base.
C   Polarization is determined from the model catalog block.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLASIN        Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      NMAPS          NMAPS         Number of input images.
C      CHANNEL        CHAN          Spectral channel, 0=> all
C      BIF            BIF           First IF to process
C      EIF            EIF           Highest IF to process
C      IN2NAME        NAME2         Name of map with CLEAN components.
C      IN2CLASS       CLAS2         Class of map with CLEAN components.
C      IN2SEQ         SEQ2          Seq. of map with CLEAN components.
C      IN2DISK        DISK2         Vol. of map with CLEAN components.
C      INVER          VER           Version no. of CC file.
C      OUTNAME        NAMOUT        Name of the output uv file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      BCOMP(64)      BCOMP         Start clean component to sub.
C                                   1 per field.
C      NCOMP(64)      NCOMP         Last Clean component no to sub.
C                                      1 per field, 0 => all
C      CMETHOD        METHOD        Modeling method:
C                                   'DFT' = FDT method
C                                   'GRID' = gridded FFT method.
C                                   '    ' chose fastest.
C      CMODEL         MODEL         Model type, 'COMP'=>CC
C                                   'IMAG'=> image.
C      FACTOR         FACGRD        Multiplicative factor for CLEAN
C                                      components fluxes. (default=1.0)
C      OPCODE         OPCODE        Operation.  'DIV ' => divide;
C                                      anything else => subtract.
C      SMODEL         SMODEL        MODEL
C                                   1 = Flux density (Jy)
C                                   2 = RA offset (arcsec E. pos)
C                                   3 = Dec offset (arcsec N. pos)
C                                   4-7 model parameters
C      BADDISK        IBAD          Disk nos. to avoid for scratch files
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, CATBLK(256), CATSAV(256)
      LOGICAL   DOMSG, DOSUM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'UVSUB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSCD.INC'
      REAL BUFF4(MAXIF)
      COMMON /MAPHDR/ CATBLK
      DATA PRGM /'UVSUB '/
      DATA DOMSG, DOSUM /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVSBIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL COPY (256, CATBLK, CATSAV)
      DATDIV = DIVIDE
C                                       Process data
C                                       If Division
      IF (DIVIDE) THEN
         CALL UVMDIV (DISKIN, OLDCNO, DISKO, NEWCNO, MODEL, METHOD,
     *      DOMSG, CHAN, NCHAN, UVBLK, JBUFSZ, FRQSEL, BUFF1, BUFF2,
     *      BUFF3, BUFF4, IRET)
C                                       Else Subtraction
      ELSE
         CALL UVMSUB (DISKIN, OLDCNO, DISKO, NEWCNO, 0, MODEL, METHOD,
     *      CHAN, NCHAN, DOSUM, DOMSG, UVBLK, JBUFSZ, FRQSEL, BUFF1,
     *      BUFF2, BUFF3, IRET)
         END IF
      IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
C                                       If no error, update history
      CALL COPY (256, CATSAV, CATBLK)
      IF (IRET.EQ.0) CALL SUBHIS
C                                       Close down
 990  CALL DIE (IRET, BUFF1)
C
      STOP
      END
      SUBROUTINE UVSBIN (PRGN, IERR)
C-----------------------------------------------------------------------
C   UVSBIN gets input parameters for UVSUB and creates an output file
C   if necessary.
C   Inputs: PRGN   C*6       Task name
C   Output: IERR   I         Error code: non-zero => quit
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, BLANK*6, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   JERR
      INTEGER   CATBLK(256), INMETH, NPARM, IERR, I, IBUFF(512), IROUND,
     *   MXFLD
      LOGICAL   T, F, WASOME
      REAL      CATR(256)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVSUB.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATH),         (IBUFF, BUFF2)
      DATA BLANK /'      '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      MXFLD = MAXAFL
      FRQSEL = -1
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IERR = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 48 + 2 * MXFLD
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      IERR = 5
      MSGTXT = 'You are using a non-standard program'
      CALL MSGWRT (3)
C                                       Crunch input parameters.
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XCMETH, CMETH)
      CALL H2CHR (4, 1, XCMOD, CMOD)
C                                       File sequence numbers
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      SEQ2 = IROUND (XS2)
C                                       File disk numbers
      DISKIN = IROUND (XDISIN)
      DISK2 = IROUND (XDISK2)
      DISKO = IROUND (XDISO)
C                                       CC file version number
      VER = IROUND (XVER)
C                                       IFs
      BIF = IROUND (XBIF)
      BIF = MAX (1, BIF)
      EIF = IROUND (XEIF)
C                                       Default EIF when have CATBLK
C                                       Number of fields
      MFIELD = 1
      IF (XNMAPS.GT.0.0) MFIELD = IROUND (XNMAPS)
      LIMFLX = XFLUX
C                                       Start component number
      NONEG = F
      WASOME = F
      DO 8 I = 1,MFIELD
         IF (I.LE.MAXAFL) THEN
            BCOMP(I) = XBCOMP(I) + 0.1
            BCOMP(I) = MAX (BCOMP(I), 1)
            NCOMP(I) = ABS (XNCOMP(I)) + 0.1
            IF (XNCOMP(I).LE.-0.5) NONEG = T
            IF (NCOMP(I).GT.0) WASOME = T
         ELSE
            BCOMP(I) = 1
            NCOMP(I) = 0
            IF (WASOME) NCOMP(I) = 1000000000
            END IF
 8       CONTINUE
C                                       Factor
      FACGRD(1) = FACTOR
      IF (ABS(FACGRD(1)).LT.1.0E-20) FACGRD(1) = 1.0
      IF (OPCODE.EQ.'MODL') THEN
         FACGRD(1) = ABS (FACGRD(1))
         FACGRD(2) = 0.0
      ELSE
         FACGRD(2) = 1.0
         END IF
C                                       Disks to avoid for scratch
      DO 10 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 10      CONTINUE
C                                       Figure out whether to sub-
C                                       tract or divide.
      DIVIDE = OPCODE(:3).EQ.'DIV'
      DOALL4 = OPCODE.EQ.'DIV4'
C                                       Get  modeling method
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Get  model type
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, 'UV',
     *      DISKIN, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Get uv header info.
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Reject multisource files (it is
C                                       not strictly necessary to do
C                                       this but input files can not
C                                       contain more than one source
C                                       and UVSUB would need to verify
C                                       this here)
      IF (ILOCSU.GE.0) THEN
         MSGTXT = 'INPUT FILE MUST BE A SINGLE-SOURCE FILE.'
         CALL MSGWRT (8)
         MSGTXT = 'USE SPLIT TO EXTRACT DESIRED DATA.'
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
C
C                                       channel number
      CHAN = IROUND (XNCH)
      IF ((CHAN.LE.0) .OR. (CHAN.GT.CATBLK(KINAX+JLOCF))) THEN
         CHAN = 1
         NCHAN = CATBLK(KINAX+JLOCF)
      ELSE
         NCHAN = 1
         END IF
C                                       Deal with IFs
      IF (JLOCIF.GE.0) THEN
C                                       IFs will look like freq channels
C                                       to the rest of the program.
C                                       Default EIF
         EIF = MIN (EIF, CATBLK(KINAX+JLOCIF))
         BIF = MIN (BIF, CATBLK(KINAX+JLOCIF))
C                                       If EIF zero to all IFs
         IF (EIF.LE.0) EIF = CATBLK(KINAX+JLOCIF)
C                                       EIF must be >= to first IF
         EIF = MAX (BIF, EIF)
C                                       Must do all freq. for all IFs
         IF (NCHAN.LT.CATBLK(KINAX+JLOCF)) EIF = BIF
C                                       Offset CHAN for IF
         IF (JLOCIF.LT.JLOCF) THEN
C                                       do all IFs if all ch in this
C                                       axis order
            IF (NCHAN.GT.1) THEN
               BIF = 1
               EIF = CATBLK(KINAX+JLOCIF)
               END IF
            CH1 = CATBLK(KINAX+JLOCIF) * (CHAN-1) + BIF
C                                       chan first usually
         ELSE
            CH1 = CATBLK(KINAX+JLOCF) * (BIF-1) + CHAN
            END IF
C                                       Change NCHAN to include IFs
         NCHAN = NCHAN * (EIF - BIF + 1)
C                                       Change Start CHAN for   IFs
         CHAN  = CH1
C                                       Reset INCF
         INCF = MIN (INCF, INCIF)
         END IF
C                                       Check order of u,v,w
      IF (((ILOCV-ILOCU).NE.1) .OR. ((ILOCW-ILOCV).NE.1)) THEN
         WRITE (MSGTXT,1070) ILOCU, ILOCV, ILOCW
         IERR = 1
         GO TO 990
         END IF
C                                       Check Stokes
      IF (ICOR0.EQ.1) GO TO 90
      IF ((NCOR.EQ.1) .AND. ((ICOR0.EQ.-1) .OR. (ICOR0.EQ.-2) .OR.
     *   (ICOR0.EQ.-5) .OR. (ICOR0.EQ.-6))) GO TO 90
      IF ((ICOR0.EQ.-1) .AND. (CATR(KRCIC+JLOCS).EQ.-1.0)) GO TO 90
      IF ((ICOR0.EQ.-5) .AND. (CATR(KRCIC+JLOCS).EQ.-1.0)) GO TO 90
         MSGTXT = 'DOES NOT WORK ON NON-STANDARD STOKES TYPES'
         IERR = 1
         GO TO 990
C                                       Setup common for modeling
C                                       routines
C                                       Uv header block
 90   CALL COPY  (256, CATBLK, UVBLK)
      INMETH = METHOD
C                                       Make sure method='GRID' for
C                                       image model.
      IF (MODEL.GE.2) THEN
         MSGTXT = 'Warning: GRID used for Images models'
         IF (METHOD.EQ.-1) CALL MSGWRT(5)
         METHOD = 1
         END IF
C                                       Check model
      DOPTMD = ABS (SMODEL(1)) .GT. 1.0E-20
      PTFLX = SMODEL(1)
      PTRAOF = SMODEL(2)
      PTDCOF = SMODEL(3)
      PARMOD(1) = SMODEL(4)
      PARMOD(2) = SMODEL(5)
      PARMOD(3) = SMODEL(6)
      PARMOD(4) = SMODEL(7)
      ISTOKE = 1
C                                       If NONEG or DOPTMD use DFT
      IF (DOPTMD) METHOD = -1
C                                       Warn user if changing METHOD
      MSGTXT = '*** WARNING: OVERRIDING SPECIFIED COMPUTATION METHOD'
      IF ((INMETH.NE.METHOD) .AND. (INMETH.NE.0)) CALL MSGWRT (6)
C                                       Get info on model file(s)
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         CALL SETGDS (DISKIN, OLDCNO, NAME2, CLAS2, SEQ2, DISK2, MFIELD,
     *      VER, NCOMP, BCOMP, MODEL, METHOD, BUFF1, BUFF2, ISTOKE,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         IF (MODEL.EQ.3) THEN
            MSGTXT = 'Using sub-images for the source model'
         ELSE IF (MODEL.EQ.2) THEN
            MSGTXT = 'Using images for the source model'
         ELSE
            MSGTXT = 'Using Clean Component source model'
            END IF
         CALL MSGWRT (3)
         CALL FACSET (DISKIN, OLDCNO, 1, 0, MODEL, FACGRD, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT,  KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      NEWCNO = 1
      FRW(NCFILE+1) = 3
      IERR = 4
      CALL UVCREA (DISKO, NEWCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1130) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((NEWCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            MSGTXT = 'MAY OVERWRITE INPUT FILE ONLY.  QUITTING'
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1190) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                        Put input file in READ
      STAT = 'READ'
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      GO TO 999
C                                       Error messages output
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVSBIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,2X,A2,' DISK=',I3,' USID=',I4)
 1020 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('WRONG ORDER FOR U, V, W =',3I4)
 1130 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1190 FORMAT ('UVSBIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE SUBHIS
C-----------------------------------------------------------------------
C  SUBHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, KSTOKE(4)*1, AMETH*4, AMODL*4, AOPCOD*4,
     *   HILINE*72
      INTEGER   CATBLK(256), LUN1, LUN2, IERR, I, NNCHAN, NONOT
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVSUB.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      COMMON /MAPHDR/ CATBLK
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA KSTOKE /'I','Q','U','V'/
      DATA NONOT, NOTTYP /0,'  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF3, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF3,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       If point model, no model file
      IF (.NOT.DOPTMD) THEN
C                                       Model file(s)
         CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFF3,
     *      IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1020) TSKNAM, VER
         IF (MODEL.EQ.1) CALL HIADD (LUN2, HILINE, BUFF3, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Number of input images
         WRITE (HILINE,1025) TSKNAM, MFIELD
         CALL HIADD (LUN2, HILINE, BUFF3, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Add no. clean comps.
         DO 25 I = 1,MFIELD
            NCOMP(I) = NSUBG(I) - 1
            WRITE (HILINE,1021) TSKNAM, I, BCOMP(I), I, NCOMP(I)
            IF (MODEL.EQ.1) CALL HIADD (LUN2, HILINE, BUFF3, IERR)
            IF (IERR.NE.0) GO TO 100
 25         CONTINUE
         END IF
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, CATBLK(KIIMS), DISKO, LUN2,
     *   BUFF3, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Channels
      NNCHAN = NCHAN
      IF (JLOCIF.GE.0) NNCHAN = NCHAN / (EIF - BIF + 1)
      WRITE (HILINE,1026) TSKNAM, CHAN, NNCHAN
      IF (NNCHAN.GT.1) CALL HIADD (LUN2, HILINE, BUFF3, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IFs
      IF (JLOCIF.GE.0) THEN
         WRITE (HILINE,1032) TSKNAM, BIF, EIF
         CALL HIADD (LUN2, HILINE, BUFF3, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Add Stokes' type.
      WRITE (HILINE,1019) TSKNAM, KSTOKE(ISTOKE)
      CALL HIADD (LUN2, HILINE, BUFF3, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Modeling method
      AMETH = 'DFT '
      IF (METHOD.EQ.1) AMETH = 'GRID'
      WRITE (HILINE,1027) TSKNAM, AMETH
      CALL HIADD (LUN2, HILINE, BUFF3, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       OPCODE
      AOPCOD = 'SUB '
      IF (DIVIDE) AOPCOD = 'DIV '
      IF (OPCODE.EQ.'MODL') AOPCOD = 'MODL'
      WRITE (HILINE,1029) TSKNAM, AOPCOD
      CALL HIADD (LUN2, HILINE, BUFF3, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Model type
C                                       CC or image
      IF (.NOT.DOPTMD) THEN
         AMODL = 'COMP'
         IF (MODEL.EQ.2) AMODL = 'IMAG'
         IF (MODEL.EQ.3) AMODL = 'SUBI'
         WRITE (HILINE,1028) TSKNAM, AMODL
         CALL HIADD (LUN2, HILINE, BUFF3, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Point model
      ELSE
         WRITE (HILINE,1030) TSKNAM, SMODEL(1), SMODEL(2), SMODEL(3)
         CALL HIADD (LUN2, HILINE, BUFF3, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Other parameters
         WRITE (HILINE,1031) TSKNAM, SMODEL(4), SMODEL(5), SMODEL(6),
     *      SMODEL(7)
         IF (SMODEL(4).GT.0.01) CALL HIADD (LUN2, HILINE, BUFF3, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                        FACTOR
      WRITE (HILINE,1023) TSKNAM, FACGRD(1)
      CALL HIADD (LUN2, HILINE, BUFF3, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (OPCODE.EQ.'MODL') THEN
         WRITE (HILINE,1024)
         CALL HIADD (LUN2, HILINE, BUFF3, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Close history file.
 100  CALL HICLOS (LUN2, T, BUFF3, IERR)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 110
         WRITE (MSGTXT,1099)
         CALL MSGWRT (7)
C                                        Update CATBLK.
 110  CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUBHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1019 FORMAT (A6,'STOKES  = ',1H',A1,1H','    / Stokes type of model')
 1020 FORMAT (A6,'VER     = ',I6,' / CC file ver. no.')
 1021 FORMAT (A6,'BCOMP(',I2,')=',I6,', NCOMP(',I3,') =',I6,
     *   ' / First-last comp. no.')
 1023 FORMAT (A6,'FACTOR  = ',F10.3,' / Model factor')
 1024 FORMAT (A6,'/ Data replaced by model completely')
 1025 FORMAT (A6,'NMAPS   = ',I6,' / Number of model images')
 1026 FORMAT (A6,'CHANNEL = ',I6,', NCHAN=',I6,' / Channel(s)')
 1027 FORMAT (A6,'CMETHOD = ',1H',A4,1H',' / Model method')
 1028 FORMAT (A6,'CMODEL  = ',1H',A4,1H',' / Model type')
 1029 FORMAT (A6,'OPCODE  = ',1H',A4,1H',' / Operation type')
 1030 FORMAT (A6,'SMODEL  = ',F12.5,2F10.5,' / Model flux,RA,Dec')
 1031 FORMAT (A6,'          ',F12.5,2F10.5,F8.5,' / Other parms')
 1032 FORMAT (A6,'BIF     = ',I6,', EIF  =',I6,' / IF range')
 1099 FORMAT ('SUBHIS: ERROR COPYING TABLES')
      END
