self: super:

{
  gitAndTools = super.gitAndTools // {
    git-appraise = super.unstable.gitAndTools.git-appraise;
  };
}
