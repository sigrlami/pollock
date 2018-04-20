import * as React from 'react';
import { Route, IndexRoute, RouterOnContext } from 'react-router';
import { bind } from 'decko';
import Layout from './view/Layout/Layout';
import { IModule, IReduxState } from 'shared/types/app';
import { EditPoll, actions as editPollActions } from 'features/editPoll';
import { PollsList, ExplorePoll, actions as viewPollsActions } from 'features/viewPolls';
import Namespace from './namespace';
import { Store } from 'redux';

class HomeModule implements IModule<Namespace.ReduxState> {
  public router: RouterOnContext | null = null;
  public store: Store<IReduxState> | null = null;

  public getRoutes() {
    const EditPollComponent = ({ params }) =>
      <EditPoll onPollSaved={this.onPollSaved} onPollRemoved={this.onPollRemoved} pollId={params.pollId} />;

    const PollsListComponent = () =>
      <PollsList onGenerateClick={this.onGeneratePollClick} editPoll={this.editPoll} openPoll={this.openPoll} />;

    const ExplorePollComponent = ({ params }) => <ExplorePoll pollId={params.pollId} />;

    return (
      <Route key="questions" path="questions" component={Layout}>
        <Route path="create" component={EditPoll} />
        <Route path="list" component={PollsListComponent} />
        <Route path=":pollId">
          <IndexRoute component={ExplorePollComponent} />
          <Route path="edit" component={EditPollComponent} />
        </Route>
      </Route>
    );
  }

  @bind
  private onGeneratePollClick() {
    if (this.store) {
      this.store.dispatch(editPollActions.generatePolls(3));
      this.store.dispatch(viewPollsActions.loadPolls());
    }
  }

  @bind
  private onPollSaved() {
    this.goTo('/questions/list');
  }

  @bind
  private onPollRemoved() {
    this.goTo('/questions/list');
  }

  @bind
  private openPoll(id: string) {
    this.goTo(`/questions/${id}`);
  }

  @bind
  private editPoll(id: string) {
    this.goTo(`/questions/${id}/edit`);
  }

  private goTo(path: string) {
    if (this.router) {
      this.router.push(path);
    }
  }
}

export default HomeModule;
