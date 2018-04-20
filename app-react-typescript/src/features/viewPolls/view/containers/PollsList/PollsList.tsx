import * as React from 'react';
import { IPoll, IReduxState } from 'shared/types/app';
import { bind } from 'decko';
import { connect } from 'react-redux';
import { bindActionCreators } from 'redux';
import { Button } from 'react-bootstrap';
import PollItem from '../../components/PollItem/PollItem';
import * as actions from './../../../redux/actions';
import * as block from 'bem-cn';
import './PollsList.styl';

interface IStateProps {
  polls: IPoll[];
}

interface IDispatchProps {
  loadPolls: typeof actions.loadPolls;
}

interface IOwnProps {
  openPoll?: (id: string) => void;
  editPoll?: (id: string) => void;
  onGenerateClick?: () => void;
}

type Props = IStateProps & IDispatchProps & IOwnProps;

function mapState(state: IReduxState): IStateProps {
  return {
    polls: state.viewPolls.polls,
  };
}

function mapDispatch(dispatch): IDispatchProps {
  return bindActionCreators({ ...actions }, dispatch);
}

class PollsList extends React.PureComponent<Props, {}> {
  private b = block('polls-list');
  public componentDidMount() {
    this.props.loadPolls();
  }

  public render() {
    const { polls, onGenerateClick } = this.props;
    return (
      <section>
        <span>
          <strong>Available polls</strong>
          <Button onClick={onGenerateClick} className={this.b('generate-action')()}>
            Generate 3 random polls
          </Button>
        </span>
        <div className={this.b('list')}>
          {polls.length ? polls.map((poll: IPoll, index: number) =>
            <PollItem key={index} {...poll} onOpenClick={this.onPollOpenClick} onEditClick={this.onPollEditClick} />,
            ) : <p>No polls available</p>}
        </div>
      </section>
    );
  }

  @bind
  private onPollOpenClick(id) {
    const handler = this.props.openPoll;

    if (handler) {
      handler(id);
    }
  }

  @bind
  private onPollEditClick(id) {
    const handler = this.props.editPoll;

    if (handler) {
      handler(id);
    }
  }
}

export { PollsList };
export default connect<IStateProps, IDispatchProps, IOwnProps>(mapState, mapDispatch)(PollsList);
