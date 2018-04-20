import * as React from 'react';
import {IPoll, IQuestion, IAnswer, IReduxState, IPollResult, IQuestionResult} from 'shared/types/app';
import * as block from 'bem-cn';
import { FormGroup, Radio, PageHeader, Well, Button } from 'react-bootstrap';
import { connect } from 'react-redux';
import { bindActionCreators } from 'redux';
import { bind } from 'decko';
import * as actions from './../../../redux/actions';
import './ExplorePoll.styl';
import FormEvent = React.FormEvent;

interface IStateProps {
  poll: IPoll | null;
  results: IPollResult[];
}

interface IDispatchProps {
  loadPoll: typeof actions.loadPoll;
  saveAnswers: typeof actions.saveAnswers;
  selectAnswer: typeof actions.selectAnswer;
}

interface IOwnProps {
  pollId: string;
}

function mapState(state: IReduxState): IStateProps {
  const selectedPoll = state.viewPolls.selectedPoll;
  return {
    poll: selectedPoll ? selectedPoll.poll : null,
    results: selectedPoll ? selectedPoll.results : [],
  };
}

function mapDispatch(dispatch): IDispatchProps {
  return bindActionCreators({
    loadPoll: actions.loadPoll,
    selectAnswer: actions.selectAnswer,
    saveAnswers: actions.saveAnswers,
  }, dispatch);
}

type Props = IStateProps & IDispatchProps & IOwnProps;

class ExplorePoll extends React.PureComponent<Props, {}> {
  private b = block('explore-poll');

  public componentDidMount() {
    this.props.loadPoll(this.props.pollId);
  }

  public render() {
    const b = this.b;
    const { poll } = this.props;
    const canSubmit: boolean = this.isAllAnswered();

    return (
      poll ? (
        <section className={b()}>
          <PageHeader>
            <span>{poll.title}</span>
            <p className={b('description')}><small>{poll.description}</small></p>
          </PageHeader>
          <div className={b('questions')}>
            {
              poll.questions.map((question: IQuestion, index: number) => (
                <article key={index}>
                  <Well>
                    <h3>{question.text}</h3>
                    <FormGroup className={b('answers')}>
                      {question.answers.map((answer: IAnswer, _index: number) => (
                          <Radio
                            key={_index}
                            checked={answer.isSelected}
                            onChange={this.getOnAnswerSelected(index, _index)}
                          >
                            {`${answer.text} (${this.getAnswerPercentage(answer)}%)`}
                          </Radio>
                      ))}
                    </FormGroup>
                  </Well>
                </article>
              ))
            }
          </div>
          <Button block bsStyle="primary" disabled={!canSubmit} onClick={this.onSubmitPoll}>
            Submit
          </Button>
        </section>
      ) : <p>No poll loaded</p>
    );
  }

  private getAnswerPercentage(answer: IAnswer): number {
    let percentage: number = 0;
    const { results } = this.props;
    const totalAnswers = results.length;
    const curAnswerCount = results.reduce((prev: number, result: IPollResult) => {
      return prev + result.questions
          .filter((question: IQuestionResult) => question.answerId === answer.id)
          .length;
    }, 0);

    if (curAnswerCount && totalAnswers) {
      percentage = (curAnswerCount / totalAnswers) * 100;
    }

    return parseFloat(percentage.toFixed());
  }

  @bind
  private isAllAnswered(): boolean {
    const { poll } = this.props;
    let isAllAnswered: boolean = false;

    if (poll) {
      isAllAnswered = poll.questions.every(question => question.answers.some(answer => answer.isSelected));
    }

    return isAllAnswered;
  }

  @bind
  private getOnAnswerSelected(questionIndex: number, answerIndex: number) {
    return () => this.props.selectAnswer(questionIndex, answerIndex);
  }

  @bind
  private onSubmitPoll() {
    this.props.saveAnswers();
    this.props.loadPoll(this.props.pollId);
  }
}

export { ExplorePoll };
export default connect<IStateProps, IDispatchProps, IOwnProps>(mapState, mapDispatch)(ExplorePoll);
