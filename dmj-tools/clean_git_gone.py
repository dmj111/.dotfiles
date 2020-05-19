"""Removes branches whose remote tracking branch is gone.

This is mostly for repos where branches are deleted on the server
after a pull request is closed.  These branches are reported as "gone"
by `git branch -v`.

"""

import click
import git


@click.command()
@click.option(
    "-f", "--force", is_flag=True, default=False, help="Force deletion of branches"
)
@click.option("-v", "--verbose", is_flag=True, default=False)
@click.argument("repopath", default=".", type=click.Path(exists=True))
def cli(force, repopath, verbose):
    repo = git.Repo(repopath)

    branches = list_gone_branches(repo, verbose)
    if not branches:
        click.echo("No branches to cleanup")
        return

    click.echo("Branches to delete:\n")
    for b in branches:
        click.echo(f" - {b}")
    click.echo("")
    if force or click.confirm("would you like to delete the branches?"):
        click.echo("deleting!!")
        for b in branches:
            # Delete even if not merged into the current branch
            click.echo(f"{b.name}  {b.commit.hexsha[:10]}")
            b.delete(repo, b.name, force=True)


def list_gone_branches(r, verbose):
    """Find branches that have a tracking branch that is no longer valid"""
    branches = []
    for b in r.heads:
        t = b.tracking_branch()
        delete = t and not t.is_valid()
        if verbose:
            click.echo(f"{b} {t} {delete}")
        if delete:
            branches.append(b)
    return branches
