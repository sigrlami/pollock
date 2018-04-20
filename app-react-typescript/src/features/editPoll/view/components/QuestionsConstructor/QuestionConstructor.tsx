import * as React from 'react';
import { FormControl, Button, Alert } from 'react-bootstrap';
import Select from 'shared/view/elements/SelectInput/SelectInput.tsx';
import { Option } from 'shared/view/elements/SelectInput/SelectInput';
import Card from 'shared/view/elements/Card/Card';
import './QuestionConstructor.styl';
import * as block from 'bem-cn';
import { IEditableAnswer, QuestionType } from 'shared/types/app';

import FormEvent = React.FormEvent;

interface IProps {
  types: Option[];
  answers?: IEditableAnswer[];
  onTypeChange?: (type: Option) => void;
  onAddAnswerClick?: () => void;
  onAnswerTextChange?: (answerIndex: number, e: FormEvent<FormControl>) => void;
  onTextChange?: (e: FormEvent<FormControl>) => void;
  type: QuestionType;
  text: string;
}

const b = block('question-constructor');

function QuestionConstructor(
  { types, type, text, answers = [], onAddAnswerClick, onAnswerTextChange, onTypeChange, onTextChange }: IProps,
) {
  const canAddAnswer = type !== 'YesNo'; // for that type answers will create automatically

  return (
    <Card>
      <div className={b()}>
        <FormControl
          type="text"
          value={text}
          onChange={onTextChange}
          className={b('control')()}
          placeholder="Question..."
        />
        <Select
          options={types}
          onChange={onTypeChange}
          className={b('control')()}
          placeholder="Select type..."
          value={type}
        />
      </div>

      <section className={b('answers')}>
        <span><strong>Answers</strong></span>
        <div className={b('answers-list')}>
          {
            answers.map((answer: IEditableAnswer, index: number) => (
              <Card key={index}>
                <FormControl
                  type="text"
                  value={answer.text}
                  onChange={onAnswerTextChange ? onAnswerTextChange.bind(null, index) : onAnswerTextChange}
                  placeholder="Answer text..."
                  disabled={!canAddAnswer}
                />
              </Card>
            ))
          }
        </div>
        <Button block className={b('control')()} onClick={onAddAnswerClick} disabled={!canAddAnswer}>
          Add answer
        </Button>
        {
          !canAddAnswer ?
            <Alert className={b('control')()} bsStyle="success">Answers will create automatically</Alert>
            : null
        }
      </section>
    </Card>
  );
}

export { IProps };
export default QuestionConstructor;
