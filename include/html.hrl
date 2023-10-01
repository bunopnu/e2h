%%%-----------------------------------------------------------------------------
%%% Macros for Common HTML Elements.
%%%
%%% These macros makes it easy to create of HTML elements that do not require
%%% attributes. They are designed to simplify the process of generating basic
%%% HTML structures without the need for extensive specifications.
%%%-----------------------------------------------------------------------------

%% Macro Definitions for Headings
-define(eH1(X), {<<"h1">>, [], X}).
-define(eH2(X), {<<"h2">>, [], X}).
-define(eH3(X), {<<"h3">>, [], X}).
-define(eH4(X), {<<"h4">>, [], X}).
-define(eH5(X), {<<"h5">>, [], X}).
-define(eH6(X), {<<"h6">>, [], X}).

%% Macro Definitions for Paragraphs and Anchors
-define(eP(X), {<<"p">>, [], X}).
-define(eA(L, X), {<<"a">>, [{<<"href">>, L}], X}).

%% Macro Definitions for Containers
-define(eDiv(X), {<<"div">>, [], X}).
-define(eSpan(X), {<<"span">>, [], X}).

%% Macro Definitions for HTML Document Structure
-define(eHtml(X), {<<"html">>, [], X}).
-define(eBody(X), {<<"body">>, [], X}).
-define(eArticle(X), {<<"article">>, [], X}).
-define(eAside(X), {<<"aside">>, [], X}).
-define(eCaption(X), {<<"caption">>, [], X}).
-define(eDetails(X), {<<"details">>, [], X}).
-define(eHead(X), {<<"head">>, [], X}).
-define(eHeader(X), {<<"header">>, [], X}).
-define(eSummary(X), {<<"summary">>, [], X}).

%% Macro Definitions for Line Breaks and Horizontal Rules
-define(eBr(), {<<"br">>}).
-define(eHr(), {<<"hr">>}).

%% Macro Definitions for Form Elements
-define(eButton(X), {<<"button">>, [], X}).
-define(eCode(X), {<<"code">>, [], X}).
-define(eTextarea(X), {<<"textarea">>, [], X}).
-define(ePre(X), {<<"pre">>, [], X}).

%% Macro Definitions for Lists
-define(eUl(X), {<<"ul">>, [], X}).
-define(eOl(X), {<<"ol">>, [], X}).
-define(eLi(X), {<<"li">>, [], X}).

%% Macro Definitions for Tables
-define(eTable(X), {<<"table">>, [], X}).
-define(eTr(X), {<<"tr">>, [], X}).
-define(eTd(X), {<<"td">>, [], X}).
-define(eTh(X), {<<"th">>, [], X}).
