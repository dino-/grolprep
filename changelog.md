1.0.2.4 (2010-11-22)

   * Added avionics photo to page headings
   * Fixed a deployment problem with the build script


1.0.2.3 (2010-11-17)

   * Completed change of style of correct/incorrect answers for
      better visibility on some screens
   * Explicitly setting focus on the submit button of most forms
     to facilitate easier keyboard operation
   * All buttons now have bold white text, also to aid with
     visibility


1.0.2.1 (2010-11-17)

   * Correct/incorrect answer text is now bold


1.0.2.0 (2010-11-17)

   * Added new Burlington Tech Center logo
   * Changed color scheme to match new logo artwork
   * Fixed bug due to hard-coded base URL in generated pages
   * Changes to logging including logfile is now at:
     /var/log/grolprep/grolprep.log
   * Added some developer utility scripts
   * Application has been better integrated with the client's servers


1.0.1.6 (2010-10-17)

   * Added missing figures for problems 3-43f6 and 3-47f2
   * Problem 3-35e5: B was marked as correct answer, but it's really
     A. Fixed
   * Fixed stacked answers in data for 8-24c5 and 8-25c6


1.0.1.5 (2010-01-17)

   * Made some SQL queries more efficient
   * Changed button labels on the question pose and evaluation
     forms to more accurately reflect what's happening
   * Changed "Next Problem" button label on summary to more
     accurately reflect the session status
   * Fixed copyright notices including updating to 2010
   * Removed some lecacy code for an older way URI dispatching was
     being handled
   * Fixed style of Submit and Cancel buttons on the feedback form
   * Fixed tab order for controls on the setup form


1.0.1.4 (2009-11-12)

   * Had introduced major problems with Microsoft browsers. To fix
     this, much redesign was needed with the initial form, JavaScript
     and server code.
   * Fixed some issues with generated XHTML not being compliant
     with the spec
   * Needed to come up with a workaround for the &ang; character
     entity (∠ symbol) that doesn't work in Microsoft browsers. This 
     affected problems: 3-17B1 3-17B2 3-17B3 3-17B4
     3-17B5
   * Cancel button was added to the feedback form
   * This project was submitted for inclusion in the 2009-Nov issue
     of the Haskell Communities and Activities Report


1.0.1.3 (2009-10-28)

   * Feedback form is now verified with the reCAPTCHA service
   * Redesigned the study setup page and changed some of the wording
   * Added the ability for users to type or paste a custom list
     of questions for study. This is handy for developers to see
     specific problems.
   * Users are now presented with a page detailing pass summary
     between each pass of problems. This is rough, more work on it
     coming soon.
   * Repaired mangled symbols or other text in some questions
     (3-17B1 3-17B2 3-17B3 3-17B4 3-17B5 3-24C6 3-96P1 3-81L2 3-90O2
     3-90O3 3-92O1 3-92O2 3-92O3 3-92O6 3-96P2)
   * Major changes to the page flow logic, state design and URI
     dispatching that aren't apparent on the outside (but that
     we're pleased with)


1.0.1.2 (2009-10-11)

   * Fixed some CSS issues with floating elements, specifying proper
     fonts in more places
   * Added the figure illustrations that are needed by many questions
     in elements 3 and 8. These illustrations now show up on the
     relevant question pages.
   * Fixed some mangled question text
   * Additions to the feedback code to include more info for the
     developers


1.0.1.1 (2009-10-09)

   * We changed the cookie expiration time to one week. You will
     be able resume study of a session for a week after the last
     time you touched it. Sessions will no longer be lost if your
     browser is closed.
   * Redesigned study setup to separate GROL study from Radar
     Endorsement
   * Added descriptive info (like this site, but better looking)
     to the main page
   * Performed some SEO on some of the pages to assist with search
     ranking in the future
   * Major visual design overhaul


1.0.1.0 (2009-10-02)

   * The software is now in beta.
   * grolprep has undergone a major renaming. Prior to this, it
     was called fequiz.
   * Feedback is now only available from the start page
   * Added feedback thank-you page


1.0.0.6 (2009-09-18)

   * The software is still in an alpha state. All planned features
     have not yet been added. But it's functional.
   * Tuned the exam data so that some special characters display
     properly on the web
   * Put test info like element, subelement and keytopic on each
     problem page
   * Added code to do the randomly-chosen simulation tests
   * Fixed bug where user was allowed to submit a problem with no
     answers chosen
   * Now being smart about stale session cookies. It should be more
     difficult now for clients to be broken by a server upgrade.
   * Feedback form is storing data now. You can write words of
     encouragement or complaint.
