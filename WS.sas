/*********************************************************************************\
PROGRAM INFORMATION
Project : Vovici API Macros
Purpose : Bring basic functionality from Vovici API to SAS
Inputs  : 
Outputs : 
Notes   : 

PROGRAM HISTORY
2009-08-10 RK Initial program developed.
2009-08-12 WR Updated WSPersonGet to create person datasets in the WSAPI library
2009-11-03 RK WSSurveySubmissionsGetXml works.
2010-11-05 RK at some point, might need to look into TIDY or XMLLINT utilities rather than PERL voodoo.
2010-11-12 RK Added WSPG_SURVEYS logic for WSPersonGet calls.
25May2011 RK Added some additional HELPful resources when asked for.
16AUG2011 RK No longer dropping PQIP variable from stransposed submissions data set.
\*********************************************************************************/;

%macro WS(
  WEBSERVICE
, WEBSERVICEURLTRUNK=http://desktop.vovici.com
, SURVEYID=
, SURVEYSEATID=
, MAILINGLISTNAME=
, MAILINGLISTCOLUMNS=
, EDITABLESURVEYID=
, MAILINGLISTID=
, MESSAGEID=
, SUBMISSIONKEY=

, INCLUDEUDFDATA=1
, PAGESIZE=10000
, PAGENUMBER=1
, SORTCOLUMN=Recipient_Id   /* Recipient_Id | UniqueId | Email | List_Id |                */
                            /* Udf0 | Udf1 | ... | Udf19 |                                */
                            /* Date_Sent | Date_Bounced | Date_Clicked | Date_Submitted | */
                            /* Hidden | Unsubscribed                                      */
, LISTFILTERS=Active        /* Active | Unsubscribed | Deleted                            */
, STARTDATE=
, RECORDLIMIT=
, GETINCOMPLETERESPONSES=0
, ACTIVE=
, EMAILADDRESSLIST=
, UID=                      /* UniqueID                                                   */
, EA=                       /* email address                                              */
, U0=, U1=, U2=, U3=, U4=, U5=, U6=, U7=, U8=, U9=
, U10=,U11=,U12=,U13=,U14=,U15=,U16=,U17=,U18=,U19=
, REPLACELIST=0
, UPDATERECORDSFIELD=
, NONMATCHINSERT=1

, WSBID=
, CHARTWIDTH=600
, CHARTHEIGHT=400

, TRANSPOSE=TRUE

, USER=&WSUSER
, PASSWORD=&WSPASS
, DEBUG=FALSE
);
  %if %upcase("&WEBSERVICE") eq "HELP" %then %do;
    *http://desktop.vovici.com/devdocs/;
    %put ****************************************************************************************;
    %put * Vovici WebServices Implemented                                                       *;
    %put ****************************************************************************************;
    %put * WSDL                       Fetch copy of wsdl                                        *;
    %put * WSHeartbeat                check status of server                                    *;
%*    %put * WSGenerateLoginToken       Authenticates a user and generates a one-time use token for 3rd party application login. This will logout all other sessions for this person.*;
    %put * WSPersonGet                get account contents                                      *;
    %put * WSSurveyGet                Retrieve published survey XML.                            *;
    %put * WSEditableSurveyGet        Retrieve editable survey XML.                             *;
    %put * WSSurveySubmissionsGet     Get an existing survey submission for a survey.           *;
    %put * WSSurveySubmissionsGetXml  Gets the raw XML submission data.                         *;
    %put * WSMailListCampaignGet      Get Campaign Info for Mailing List Id.                    *;
    %put * WSMailListRecipientsGet    Get Campaign Recipients XML for message Id.               *;
    %put * WSSurveyUpdateStatus       Modifies a list of users survey status to Run or Stop.    *;
    %put * WSMailListRecipientsUnsubscribe                                                      *;
    %put * WSMailListCampaignRecipientsGet                                                      *;
%*    %put * WSMailingListCreate        Ought to create a new mailing list, but does not.         *;
    %put ****************************************************************************************;
	x "explorer &WEBSERVICEURLTRUNK./EFMWebTopSvc.asmx";
	x "start .\programs\";
  %end;


  %else %if %upcase("&WEBSERVICE") eq "CLEAR" %then %do;
	proc datasets library=work nodetails nolist;
		delete WS:;
		run;
	quit;

	proc sql noprint;
	  select distinct fmtname
	  into :WSB_formats separated by ' '
	  from dictionary.formats
	  where libname eq 'WORK'
	  	and memname eq 'FORMATS'
		and fmtname like 'WSB%'
		and fmttype eq 'F';

	  select distinct fmtname
	  into :WSB_informats separated by ' '
	  from dictionary.formats
	  where libname eq 'WORK'
	  	and memname eq 'FORMATS'
		and fmtname like 'WSB%'
		and fmttype eq 'I';
	quit;
	proc catalog catalog=work.formats;
		delete &WSB_informats / et=infmt;
		delete &WSB_formats / et=format;
		run;
	quit;
  %end;

  %else %if %upcase("&WEBSERVICE") eq "WSDL" %then %do;
    filename wsdl url "&WEBSERVICEURLTRUNK./EFMWebTopSvc.asmx?wsdl";
    libname wsdl xml92 xmltype=wsdl;
    proc sql;
      create table wsdl_main as
      select distinct *
      from dictionary.columns
      where libname eq 'WSDL'
      order by memname, varnum;
    quit;
    %if %upcase("&DEBUG") ne "TRUE" %then %do;
      libname wsdl clear;
      filename wsdl clear;
    %end;
  %end;

  %else %if "&WEBSERVICE" ne "" %then %do;
    %put ***********************************************************************************;
    %put Parameter values to be used by &SYSMACRONAME are:;
    %put _local_;
    %put ***********************************************************************************;

    filename _ws_req "programs\request.xml";
    filename _ws_resp "programs\response.xml";
    filename _ws_map "programs\WS.map";

    data _null_;
      infile "programs\&WEBSERVICE..txt" lrecl=1000;
      input;
      line=resolve(_infile_);
      file _ws_req;
      put line;
    run;

    proc soap
      in=_ws_req
      out=_ws_resp
      url="&WEBSERVICEURLTRUNK./EFMWebTopSvc.asmx"
      soapaction="http://desktop.vovici.com/EFMWebTopSvc/&WEBSERVICE"
      &PROXYINFO;
    run;

    %let perl=FALSE;
    %let plTidy=FALSE;
    %let plQuotes=FALSE;
    %let plUnencapsulate=FALSE;
    %let plSkipcdata=FALSE;

    %if %upcase("&WEBSERVICE") eq "WSSURVEYGET"
     or %upcase("&WEBSERVICE") eq "WSMAILLISTRECIPIENTSGET"
     or %upcase("&WEBSERVICE") eq "WSMAILLISTCAMPAIGNRECIPIENTSGET"
     or %upcase("&WEBSERVICE") eq "WSEDITABLESURVEYGET"
     or %upcase("&WEBSERVICE") eq "WSSURVEYSUBMISSIONSGETXML"
      %then %do;
      %let perl=TRUE;
      %let plTidy=TRUE;
      %let plUnencapsulate=TRUE;
    %end;

    %else %if %upcase("&WEBSERVICE") eq "WSSURVEYSUBMISSIONSGET" %then %do;
      %let perl=TRUE;
    %end;

    %else %if %upcase("&WEBSERVICE") eq "WSMAILLISTCAMPAIGNGET" %then %do;
      %let perl=TRUE;
      %let plQuotes=TRUE;
    %end;

    %if %upcase("&PERL") ne "FALSE" %then %do;
      x "perl ""programs\fixVovici.pl"" ""programs\response"" &PLTIDY &PLQUOTES &PLUNENCAPSULATE &PLSKIPCDATA ";
      filename _ws_resp "programs\response.parsed.xml";
    %end;

    proc format; invalue TF (upcase) 'TRUE'=1 'FALSE'=0 other=. ; run;

    libname _ws_resp xml xmlmap=_ws_map access=READONLY;

    proc datasets library=work nodetails nolist; delete %sysfunc(compress(&WEBSERVICE,,ku))_:; run; quit;

    proc copy in=_ws_resp out=work; select %sysfunc(compress(&WEBSERVICE,,ku))_:; run;

    libname _ws_resp clear;
    filename _ws_map clear;
    filename _ws_resp clear;
    filename _ws_req clear;

    %if %upcase("&WEBSERVICE") eq "WSSURVEYGET" %then %do;
    /*create a survey codebook*/
		data wssg_q;
			format wsb $20.;
			if 0 then set wssg_q3;
			set wssg_q1 wssg_q2 wssg_q3;

			if disabled ne 1;

			wsb=cats('wsb', id_q1);
			if id_q2 ne . then wsb=catx('_',wsb,id_q2);
			if id_q3 ne . then wsb=catx('_',wsb,id_q3);

			select (style);
				when ('qsHidden') html_text=cats('{', data_name, '}');
				when ('qsPageBreak') do;
					if jump_logic_type ne 1
						then html_text='{page break (simple)}';
						else html_text=cats('{page break (skip to wsb', jump_logic_destination,')}');
					end;
				otherwise;
			end;
		run;

		proc sort; by seq_q1-seq_q3; run;

		data wssg_r;
			format wsb $20. id_r 4.;
			if 0 then set wssg_r3;
			set wssg_r1 wssg_r2 wssg_r3;

			wsb=cats('wsb',id_q1);
			if id_q2 ne . then wsb=catx('_',wsb,id_q2);
			if id_q3 ne . then wsb=catx('_',wsb,id_q3);

			id_r=coalesce(id_r3, id_r2, id_r1);
			if missing(page_break_expression)=0 then html_text=cats('{',page_break_expression,'}');
		run;

		proc sort; by seq_q1-seq_q3 seq_r1-seq_r3; run;

		data wssg_qr;
			if 0 then set wssg_r(keep=seq_: id_: );
			set wssg_q wssg_r;
			if _n_=1 then do;
				re_popen    =prxparse('s/\<p>//');
				re_pclose   =prxparse('s/\<\/p>/ /');
				re_quote    =prxparse('s/"/''/');
				re_strong   =prxparse('s/\<\/?(strong|b)\>/*/');
				re_emphasis =prxparse('s/\<\/?(em|i)\>/_/');
				re_amp      =prxparse('s/\&amp;/&/');
				re_nbsp     =prxparse('s/\&nbsp;/ /');
				re_br       =prxparse('s/\<br( \/)?\>/ /');
				re_style    =prxparse('s/\<style\>.*\<\/style\>//');
				re_header   =prxparse('s/\<h\d\>(.*)\<\/h\d>/~H~$1/');
				retain re_:;
			end;

			array t(*) html_text;
			array re(*) re_:;
			do i = 1 to dim(t);
				do r = 1 to dim(re);
					t[i]=prxchange(re[r], -1, t[i]);
				end;
				if substr(t[i], 1, 3) eq '~H~' then t[i] = upcase(substr(t[i], 4));
			end;
			drop i r re_:;
		run;

		proc sort; by wsb seq_q: seq_r:; run;

		proc sort; by seq_q1; run;

		data wssg_qr;
			set wssg_qr;
			format style_hold $16.;
			retain style_hold;
			style_hold=coalescec(style, style_hold);
			if style eq '' then style=style_hold;
			drop style_hold;

			/*now, some fixes*/
			if style eq 'qsMultipleSelect' and id_r ne . then do;
				wsb=catx('_', wsb, id_r);
				score=1;
				end;
			if style eq 'qsMatrix' and id_r ne . then delete;
		run;

		/* build the codebook stuff */
		proc sql;
			* these questions are implicitly or explicitly scored ;
			create table scoredquestions as
			select distinct wsb, seq_q1, seq_q2, seq_q3
			from wssg_qr
			where wsb in (select distinct wsb from wssg_qr where style in ('qsSingleSelect', 'qsMultipleSelect') and score ne .)
				or wsb in (select distinct wsb from wssg_qr where style in ('qsNumeric'))
			order by 2, 3, 4;

			* and here is the scoring for the responses to those questions ;
			create table scoredresponses as
			select distinct *
			from wssg_qr(keep=id_r--html_text code score)
			where wsb in (select wsb from scoredquestions)
				and (	(style ne 'qsNumeric' and id_r ne .)
					or 
						(style eq 'qsNumeric')
					)
			order by seq_q1, seq_q2, seq_q3, seq_r1, seq_r2, seq_r3;

			* these are closed-ended questions that are not scored ;
			create table unscoredquestions as
			select distinct wsb, seq_q1, seq_q2, seq_q3
			from wssg_qr
			where wsb in (select distinct wsb from wssg_qr where style in ('qsSingleSelect', 'qsMultipleSelect') and score eq .)
				and wsb not in (select wsb from scoredquestions)
			order by 2, 3, 4;

			* and here is the coding for the responses to those questions ;
			create table unscoredresponses as
			select distinct *
			from wssg_qr(keep=)
			where wsb not in(select wsb from scoredquestions)
				and wsb in (select wsb from unscoredquestions)
				and id_r ne .
			order by seq_q1, seq_q2, seq_q3, seq_r1, seq_r2, seq_r3;
		quit;

		data wsbformats(keep=fmtname type start label hlo);
			format
				fmtname $32.
				type $1.
				start $16.
				label $30.
				HLO $8.;

			set scoredresponses;
			where (style ne 'qsNumeric' and id_r ne .)
				or (style eq 'qsNumeric');

			if style eq 'qsNumeric' then do;
				type='I';
				fmtname=cats(wsb, type);
				start='**OTHER**';
				label='[BEST40.]';
				hlo='OF';
				output;

				type='N';
				fmtname=cats(wsb, type);
				output;
			end;
			else do;
				if score eq . then score=.N;
				if code ne . then score=code;

				type='I';
				fmtname=cats(wsb, type);
				start=left(put(id_r,8.));
				if missing(score) and score ne .	then label=cats('.', score);
													else label=left(put(score,8.));
				output;

				type='N';
				if style eq 'qsMultipleSelect' then html_text='1';
				fmtname=cats(wsb, type);
				if missing(score) and score ne .	then start=cats('.',score);
													else start=left(label);
				label=left(htmldecode(html_text));
				output;

				if style eq 'qsMultipleSelect' then do;
					start='**OTHER**';
					label='0';
					hlo='OF';
					output;
				end;
			end;
		run;
		proc sort; by fmtname type start label; run;
		proc format cntlin=wsbformats; run;

	%end;

    %else %if %upcase("&WEBSERVICE") eq "WSMAILLISTCAMPAIGNGET" %then %do;
      *****check to see that WSPERSONGET_SURVEYINFO exists?;
      *fetch surveyid;
      proc sql;
        create table WSMLCG_main as
        select distinct s.SurveyID, c.*
        from WSMLCG_main as c
        left join WSPG_SurveyInfo as s
        on scan(scan(c.LinkList, -1, '/'), 1, '?') eq scan(s.SurveyURL, -1, '/');
      quit;

      *base rates and relative rates;
      data WSMLCG_main;
        set WSMLCG_main;
        if MessageType in ('New', 'Reminder') and BaseCount > 1 then do;
			BaseBounceRate=BounceCount / BaseCount;
			BaseSuccessRate=SuccessCount / BaseCount; 
			BaseClickRate=ClickCount / BaseCount;
			BaseSubmitRate = SubmitCount / BaseCount;

			SuccessClickRate=ClickCount / SuccessCount;
			ClickSubmitRate=SubmitCount / ClickCount;
		end;
        format BaseBounceRate--ClickSubmitRate percent8.1;
      run;
    %end;

    %else %if %upcase("&WEBSERVICE") eq "WSMAILLISTCAMPAIGNRECIPIENTSGET" %then %do;
      data WSMLCRG_main(drop=i re_: _d:);
        format rid 8.
               uid ea $50.
               u0-u19 $200.
               ds db dc dsb datetime.
               em $500.
               ec pf 4.;
        set WSMLCRG_main;

        if _n_=1 then do;
          re_dt = prxparse('/(\d+)\/(\d+)\/(\d\d\d\d)\s+(\d+):(\d\d):(\d\d)\s+([AP]M)/');
          re_err= prxparse("/\((\d\d\d)\).*/");
          retain re_:;
        end;

        array ndt(*)  ds  db  dc  dsb;
        array cdt(*) _ds _db _dc _dsb;
        do i = 1 to dim(cdt);
          if (prxmatch(re_dt, cdt[i])) then
            ndt[i]=dhms(mdy(input(prxposn(re_dt,1,cdt[i]),2.)  /*month*/
                          , input(prxposn(re_dt,2,cdt[i]),2.)  /*day*/
                          , input(prxposn(re_dt,3,cdt[i]),4.)  /*year*/)
                      , input(prxposn(re_dt,4,cdt[i]),2.)      /*hour*/
                      + 12 * (prxposn(re_dt,7,cdt[i]) eq 'PM') /*pm offset*/
                      , input(prxposn(re_dt,5,cdt[i]),2.)      /*minute*/
                      , input(prxposn(re_dt,6,cdt[i]),2.)      /*second*/);
        end;

        if (em ne '') then do;
          if prxmatch(re_err, em)
            then ec = input(prxposn(re_err, 1, em), 8.);
            else ec=.U;
        end;
        else ec=.;
        %if &INCLUDEUDFDATA ~= 1 %then drop u0-u19;;
      run;
    %end;

	%else %if %upcase("&WEBSERVICE") eq "WSPERSONGET" %then %do;
		proc sort data=wspg_editablesurvey;
			by EditableSurveyID;
		run;
		proc sort data=wspg_surveyinfo;
			by EditableSurveyID;
		run;
		data wspg_surveys;
			if 0 then set wspg_surveyinfo;
			merge wspg_editablesurvey wspg_surveyinfo;
			by EditableSurveyID;
		run;
	%end;

    %else %if %upcase("&WEBSERVICE") eq "WSSURVEYSUBMISSIONSGETXML" %then %do;
		%global informat_list rename_list recode_list label_list hidden_list multiselect_list numeric_list;
		proc sql noprint;
			%let informat_list=;
			select distinct fmtname as Informat_List
			into :informat_list separated by ' '
			from wsbformats
			where type eq 'I';

			%let rename_list=;
			select catx('=_', wsb, wsb) as Rename_List
			into :rename_list separated by ' '
			from scoredquestions
			where wsb in (select distinct n from wsssgx_main);

			%let recode_list=;
			select distinct cats(wsb, '=input(_', wsb, ',', wsb, 'i.);') as Recode_List
			into :recode_list separated by ' '
			from scoredquestions
			order by seq_q1, seq_q2, seq_q3;

			%let label_list=;
			select distinct cats(wsb,'="', substr(htmldecode(html_text), 1, 188), '"') as Label_List
			into :label_list separated by ' '
			from wssg_qr
			where 	(id_r=. and style not in ('qsMultipleSelect','qsPageBreak','qsHtmlHeader','qsMatrix','qsDataBlock',' ','qsContainer','qsHidden'))
					or
					(id_r ne . and style eq 'qsMultipleSelect')
				and layout eq '';

			%let hidden_list=;
			select distinct wsb as Hidden_List
			into :hidden_list separated by ' '
			from wssg_q
			where style eq 'qsHidden';

			%let multiselect_list=;
			select distinct wsb as Multiselect_list
			into :multiselect_list separated by ' '
			from scoredresponses
			where style eq 'qsMultipleSelect';

			%let numeric_list=;
			select distinct wsb as Numeric_List
			into :numeric_list separated by ' '
			from wssg_q
			where style eq 'qsNumeric';
		quit;

		%if %upcase("&TRANSPOSE") eq "TRUE" %then %do;
			proc transpose
				data=WSSSGX_Main
				out=WSSSGX_Data(drop=_: renderlang);
			by SurveyID S K notsorted;
			id n;
			var v;
			run;

			data WSSSGX_data;
				format SurveyID 8.
					S datetime19.
					K 8.
					UniqueID $50.
					RemoteAddr $15.
					WEM $75.
					pqip $100.;
			set WSSSGX_Data;
			run;
		%end;
    %end;

    %if %upcase("&DEBUG") ne "TRUE" %then %do;
      x 'del "programs\request*.xml"';
      x 'del "programs\response*.xml"';
    %end;
  %end;
%mend WS;
