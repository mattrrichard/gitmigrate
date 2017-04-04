# Git Migration Automation Script

We were slowly migrating things to Bitbucket as we touched them,
but it was annoying having things in two places, so I created this to automate the move.

## Messyness Side note
This was a "for fun" project so I over-engineered the hell out of it just to explore some
parts of Haskell that I haven't spent a lot of time on. There are parts of this that are 
really rough-- where I would do it pretty differently if I did it again. However, given that it
did the work we needed, and we won't need it until the next time we move all of our projects 
to a new git host, I haven't really gone back to fix the rough edges.  

## So, what does it do?

Given a directory that contains a bunch of git repositories, it does these things for each git repo:
  * Check that the repo hasn't already been moved (e.g. origin doesn't contain "bitbucket")
  * Check that there isn't a repo in our bitbucket account with that name already
  * Look up the VCS root id in our Teamcity instance
  * Create a repo on bitbucket with our deployment key in place
  * Change origin in the local repo and push
  * Update the Teamcity VCS root to point to the new bitbucket url
